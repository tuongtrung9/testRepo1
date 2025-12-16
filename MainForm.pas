unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, 
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, 
  Vcl.StdCtrls, REST.Types, REST.Client, Data.Bind.Components, 
  Data.Bind.ObjectScope, REST.Authenticator.OAuth, System.JSON,
  Vcl.ComCtrls, Vcl.ExtCtrls, System.Threading;

type
  TFormMain = class(TForm)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    OAuth2Authenticator1: TOAuth2Authenticator;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    EditEmail: TEdit;
    EditPassword: TEdit;
    btnLogin: TButton;
    GroupBox2: TGroupBox;
    btnGetUsers: TButton;
    MemoResponse: TMemo;
    btnRefreshToken: TButton;
    btnClearTokens: TButton;
    StatusBar1: TStatusBar;
    Label3: TLabel;
    LabelTokenStatus: TLabel;
    PanelLoading: TPanel;
    ShapeLoading: TShape;
    LabelLoading: TLabel;
    ProgressBarLoading: TProgressBar;
    TimerLoading: TTimer;
    btnCancelRequest: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnRefreshTokenClick(Sender: TObject);
    procedure btnGetUsersClick(Sender: TObject);
    procedure btnClearTokensClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerLoadingTimer(Sender: TObject);
    procedure btnCancelRequestClick(Sender: TObject);
  private
    FTask: ITask;
    FCancelled: Boolean;
    FLoadingAnimationStep: Integer;
    
    procedure ShowLoading(const Message: string);
    procedure HideLoading;
    procedure EnableControls(AEnable: Boolean);
    procedure UpdateTokenStatus;
    function IsTokenExpired: Boolean;
    
    procedure LoginAsync(const Email, Password: string);
    procedure RefreshAccessTokenAsync;
    procedure GetUsersListAsync;
    
    function Login(const Email, Password: string): Boolean;
    function RefreshAccessToken: Boolean;
    function GetUsersList: string;
    procedure SaveTokensFromResponse(const JSONResponse: TJSONObject);
    
    procedure LogMessage(const Msg: string);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  ApiEndpoints, TokenStorage, System.DateUtils;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FCancelled := False;
  FLoadingAnimationStep := 0;
  
  TTokenStorage.Initialize;
  
  RESTClient1.BaseURL := API_BASE_URL;
  RESTClient1.ConnectTimeout := 30000;
  RESTClient1.ReadTimeout := 30000;
  
  RESTRequest1.Client := RESTClient1;
  RESTRequest1.Response := RESTResponse1;
  RESTRequest1.Timeout := 30000;
  
  OAuth2Authenticator1.AccessToken := TTokenStorage.LoadAccessToken;
  OAuth2Authenticator1.TokenType := TOAuth2TokenType.ttBEARER;
  RESTClient1.Authenticator := OAuth2Authenticator1;
  
  PanelLoading.Visible := False;
  PanelLoading.BringToFront;
  
  UpdateTokenStatus;
  
  StatusBar1.SimpleText := 'Safe Mode: ' + 
    IfThen(TTokenStorage.IsSafeMode, 'ENABLED (DPAPI)', 'DISABLED (Memory Only)');
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  TimerLoading.Enabled := False;
  
  if Assigned(FTask) then
  begin
    FCancelled := True;
    Sleep(100);
  end;
end;

procedure TFormMain.ShowLoading(const Message: string);
begin
  FCancelled := False;
  FLoadingAnimationStep := 0;
  LabelLoading.Caption := Message;
  ProgressBarLoading.Position := 0;
  PanelLoading.Visible := True;
  PanelLoading.BringToFront;
  TimerLoading.Enabled := True;
  Application.ProcessMessages;
end;

procedure TFormMain.HideLoading;
begin
  TimerLoading.Enabled := False;
  PanelLoading.Visible := False;
  Application.ProcessMessages;
end;

procedure TFormMain.EnableControls(AEnable: Boolean);
begin
  btnLogin.Enabled := AEnable;
  btnRefreshToken.Enabled := AEnable;
  btnGetUsers.Enabled := AEnable;
  btnClearTokens.Enabled := AEnable;
  EditEmail.Enabled := AEnable;
  EditPassword.Enabled := AEnable;
  btnCancelRequest.Visible := not AEnable;
end;

procedure TFormMain.TimerLoadingTimer(Sender: TObject);
begin
  Inc(FLoadingAnimationStep);
  
  ProgressBarLoading.Position := (FLoadingAnimationStep * 5) mod 100;
  
  case (FLoadingAnimationStep div 2) mod 4 of
    0: ShapeLoading.Brush.Color := clBlue;
    1: ShapeLoading.Brush.Color := clAqua;
    2: ShapeLoading.Brush.Color := clBlue;
    3: ShapeLoading.Brush.Color := clSkyBlue;
  end;
end;

procedure TFormMain.btnCancelRequestClick(Sender: TObject);
begin
  FCancelled := True;
  LogMessage('Request cancellation requested...');
end;

procedure TFormMain.LogMessage(const Msg: string);
begin
  TThread.Synchronize(nil, 
    procedure
    begin
      MemoResponse.Lines.Add('[' + FormatDateTime('hh:nn:ss', Now) + '] ' + Msg);
    end
  );
end;

procedure TFormMain.UpdateTokenStatus;
var
  AccessToken: string;
  Expiry: TDateTime;
begin
  AccessToken := TTokenStorage.LoadAccessToken;
  Expiry := TTokenStorage.LoadTokenExpiry;
  
  if AccessToken <> '' then
  begin
    if IsTokenExpired then
      LabelTokenStatus.Caption := 'Token Status: EXPIRED'
    else
      LabelTokenStatus.Caption := 'Token Status: VALID (expires: ' + 
        FormatDateTime('yyyy-mm-dd hh:nn:ss', Expiry) + ')';
    
    LabelTokenStatus.Font.Color := IfThen(IsTokenExpired, clRed, clGreen);
  end
  else
  begin
    LabelTokenStatus.Caption := 'Token Status: NOT AUTHENTICATED';
    LabelTokenStatus.Font.Color := clGray;
  end;
end;

function TFormMain.IsTokenExpired: Boolean;
var
  Expiry: TDateTime;
begin
  Expiry := TTokenStorage.LoadTokenExpiry;
  Result := (Expiry = 0) or (Now >= Expiry);
end;

procedure TFormMain.SaveTokensFromResponse(const JSONResponse: TJSONObject);
var
  AccessToken, RefreshToken: string;
  ExpiresIn: Integer;
  ExpiryTime: TDateTime;
begin
  AccessToken := JSONResponse.GetValue<string>('accessToken');
  RefreshToken := JSONResponse.GetValue<string>('refreshToken');
  ExpiresIn := JSONResponse.GetValue<Integer>('expiresin');
  
  ExpiryTime := IncSecond(Now, ExpiresIn - 30);
  
  TTokenStorage.SaveAccessToken(AccessToken);
  TTokenStorage.SaveRefreshToken(RefreshToken);
  TTokenStorage.SaveTokenExpiry(ExpiryTime);
  
  OAuth2Authenticator1.AccessToken := AccessToken;
  
  UpdateTokenStatus;
end;

function TFormMain.Login(const Email, Password: string): Boolean;
var
  JSONBody: TJSONObject;
  JSONResponse: TJSONObject;
  LocalRESTClient: TRESTClient;
  LocalRESTRequest: TRESTRequest;
  LocalRESTResponse: TRESTResponse;
begin
  Result := False;
  
  LocalRESTClient := TRESTClient.Create(nil);
  LocalRESTResponse := TRESTResponse.Create(nil);
  LocalRESTRequest := TRESTRequest.Create(nil);
  try
    LocalRESTClient.BaseURL := API_BASE_URL;
    LocalRESTClient.ConnectTimeout := 30000;
    LocalRESTClient.ReadTimeout := 30000;
    
    LocalRESTRequest.Client := LocalRESTClient;
    LocalRESTRequest.Response := LocalRESTResponse;
    LocalRESTRequest.Timeout := 30000;
    
    JSONBody := TJSONObject.Create;
    try
      JSONBody.AddPair('email', Email);
      JSONBody.AddPair('password', Password);
      
      LocalRESTRequest.Method := TRESTRequestMethod.rmPOST;
      LocalRESTRequest.Resource := ENDPOINT_LOGIN;
      LocalRESTRequest.ClearBody;
      LocalRESTRequest.AddBody(JSONBody.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
      
      LogMessage('Sending login request...');
      
      if FCancelled then Exit;
      LocalRESTRequest.Execute;
      
      if FCancelled then
      begin
        LogMessage('Login request cancelled');
        Exit;
      end;
      
      if LocalRESTResponse.StatusCode = 200 then
      begin
        JSONResponse := TJSONObject.ParseJSONValue(LocalRESTResponse.Content) as TJSONObject;
        try
          TThread.Synchronize(nil,
            procedure
            begin
              SaveTokensFromResponse(JSONResponse);
            end
          );
          Result := True;
          LogMessage('Login successful!');
          LogMessage('Response: ' + LocalRESTResponse.Content);
        finally
          JSONResponse.Free;
        end;
      end
      else
      begin
        LogMessage('Login failed!');
        LogMessage('Status: ' + LocalRESTResponse.StatusCode.ToString);
        LogMessage('Response: ' + LocalRESTResponse.Content);
      end;
    finally
      JSONBody.Free;
    end;
  except
    on E: Exception do
    begin
      LogMessage('Exception during login: ' + E.Message);
    end;
  end;
  
  LocalRESTRequest.Free;
  LocalRESTResponse.Free;
  LocalRESTClient.Free;
end;

function TFormMain.RefreshAccessToken: Boolean;
var
  JSONBody: TJSONObject;
  JSONResponse: TJSONObject;
  RefreshToken: string;
  LocalRESTClient: TRESTClient;
  LocalRESTRequest: TRESTRequest;
  LocalRESTResponse: TRESTResponse;
begin
  Result := False;
  
  RefreshToken := TTokenStorage.LoadRefreshToken;
  if RefreshToken = '' then
  begin
    LogMessage('No refresh token available!');
    Exit;
  end;
  
  LocalRESTClient := TRESTClient.Create(nil);
  LocalRESTResponse := TRESTResponse.Create(nil);
  LocalRESTRequest := TRESTRequest.Create(nil);
  try
    LocalRESTClient.BaseURL := API_BASE_URL;
    LocalRESTClient.ConnectTimeout := 30000;
    LocalRESTClient.ReadTimeout := 30000;
    
    LocalRESTRequest.Client := LocalRESTClient;
    LocalRESTRequest.Response := LocalRESTResponse;
    LocalRESTRequest.Timeout := 30000;
    
    JSONBody := TJSONObject.Create;
    try
      JSONBody.AddPair('refreshToken', RefreshToken);
      
      LocalRESTRequest.Method := TRESTRequestMethod.rmPOST;
      LocalRESTRequest.Resource := ENDPOINT_REFRESH_TOKEN;
      LocalRESTRequest.ClearBody;
      LocalRESTRequest.AddBody(JSONBody.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
      
      LogMessage('Refreshing access token...');
      
      if FCancelled then Exit;
      LocalRESTRequest.Execute;
      
      if FCancelled then
      begin
        LogMessage('Refresh token request cancelled');
        Exit;
      end;
      
      if LocalRESTResponse.StatusCode = 200 then
      begin
        JSONResponse := TJSONObject.ParseJSONValue(LocalRESTResponse.Content) as TJSONObject;
        try
          TThread.Synchronize(nil,
            procedure
            begin
              SaveTokensFromResponse(JSONResponse);
            end
          );
          Result := True;
          LogMessage('Token refreshed successfully!');
          LogMessage('Response: ' + LocalRESTResponse.Content);
        finally
          JSONResponse.Free;
        end;
      end
      else
      begin
        LogMessage('Token refresh failed!');
        LogMessage('Status: ' + LocalRESTResponse.StatusCode.ToString);
        LogMessage('Response: ' + LocalRESTResponse.Content);
      end;
    finally
      JSONBody.Free;
    end;
  except
    on E: Exception do
    begin
      LogMessage('Exception during token refresh: ' + E.Message);
    end;
  end;
  
  LocalRESTRequest.Free;
  LocalRESTResponse.Free;
  LocalRESTClient.Free;
end;

function TFormMain.GetUsersList: string;
var
  LocalRESTClient: TRESTClient;
  LocalRESTRequest: TRESTRequest;
  LocalRESTResponse: TRESTResponse;
  CurrentAccessToken: string;
begin
  Result := '';
  
  if IsTokenExpired then
  begin
    LogMessage('Token expired, refreshing...');
    if not RefreshAccessToken then
    begin
      LogMessage('Failed to refresh token. Please login again.');
      Exit;
    end;
  end;
  
  LocalRESTClient := TRESTClient.Create(nil);
  LocalRESTResponse := TRESTResponse.Create(nil);
  LocalRESTRequest := TRESTRequest.Create(nil);
  try
    LocalRESTClient.BaseURL := API_BASE_URL;
    LocalRESTClient.ConnectTimeout := 30000;
    LocalRESTClient.ReadTimeout := 30000;
    
    CurrentAccessToken := TTokenStorage.LoadAccessToken;
    
    LocalRESTRequest.Client := LocalRESTClient;
    LocalRESTRequest.Response := LocalRESTResponse;
    LocalRESTRequest.Timeout := 30000;
    
    LocalRESTRequest.Method := TRESTRequestMethod.rmGET;
    LocalRESTRequest.Resource := ENDPOINT_USERS_LIST;
    LocalRESTRequest.Params.Clear;
    
    LocalRESTRequest.Params.AddHeader('Authorization', 'Bearer ' + CurrentAccessToken);
    
    LogMessage('Fetching users list...');
    
    if FCancelled then Exit;
    LocalRESTRequest.Execute;
    
    if FCancelled then
    begin
      LogMessage('Get users request cancelled');
      Exit;
    end;
    
    if LocalRESTResponse.StatusCode = 200 then
    begin
      Result := LocalRESTResponse.Content;
      LogMessage('Users list retrieved successfully!');
      LogMessage('Response: ' + Result);
    end
    else if LocalRESTResponse.StatusCode = 401 then
    begin
      LogMessage('Unauthorized, attempting to refresh token...');
      if RefreshAccessToken then
      begin
        CurrentAccessToken := TTokenStorage.LoadAccessToken;
        LocalRESTRequest.Params.Clear;
        LocalRESTRequest.Params.AddHeader('Authorization', 'Bearer ' + CurrentAccessToken);
        
        if FCancelled then Exit;
        LocalRESTRequest.Execute;
        
        if not FCancelled and (LocalRESTResponse.StatusCode = 200) then
        begin
          Result := LocalRESTResponse.Content;
          LogMessage('Users list retrieved after token refresh!');
          LogMessage('Response: ' + Result);
        end;
      end
      else
      begin
        LogMessage('Failed to refresh token. Please login again.');
      end;
    end
    else
    begin
      LogMessage('Failed to get users list!');
      LogMessage('Status: ' + LocalRESTResponse.StatusCode.ToString);
      LogMessage('Response: ' + LocalRESTResponse.Content);
    end;
  except
    on E: Exception do
    begin
      LogMessage('Exception during get users: ' + E.Message);
    end;
  end;
  
  LocalRESTRequest.Free;
  LocalRESTResponse.Free;
  LocalRESTClient.Free;
end;

procedure TFormMain.LoginAsync(const Email, Password: string);
begin
  EnableControls(False);
  ShowLoading('Authenticating...');
  
  FTask := TTask.Run(
    procedure
    begin
      try
        Login(Email, Password);
      finally
        TThread.Synchronize(nil,
          procedure
          begin
            HideLoading;
            EnableControls(True);
          end
        );
      end;
    end
  );
end;

procedure TFormMain.RefreshAccessTokenAsync;
begin
  EnableControls(False);
  ShowLoading('Refreshing token...');
  
  FTask := TTask.Run(
    procedure
    begin
      try
        RefreshAccessToken;
      finally
        TThread.Synchronize(nil,
          procedure
          begin
            HideLoading;
            EnableControls(True);
          end
        );
      end;
    end
  );
end;

procedure TFormMain.GetUsersListAsync;
begin
  EnableControls(False);
  ShowLoading('Loading users...');
  
  FTask := TTask.Run(
    procedure
    begin
      try
        GetUsersList;
      finally
        TThread.Synchronize(nil,
          procedure
          begin
            HideLoading;
            EnableControls(True);
          end
        );
      end;
    end
  );
end;

procedure TFormMain.btnLoginClick(Sender: TObject);
begin
  MemoResponse.Clear;
  if (EditEmail.Text <> '') and (EditPassword.Text <> '') then
    LoginAsync(EditEmail.Text, EditPassword.Text)
  else
    ShowMessage('Please enter email and password');
end;

procedure TFormMain.btnRefreshTokenClick(Sender: TObject);
begin
  MemoResponse.Clear;
  RefreshAccessTokenAsync;
end;

procedure TFormMain.btnGetUsersClick(Sender: TObject);
begin
  MemoResponse.Clear;
  GetUsersListAsync;
end;

procedure TFormMain.btnClearTokensClick(Sender: TObject);
begin
  TTokenStorage.ClearAllTokens;
  OAuth2Authenticator1.AccessToken := '';
  UpdateTokenStatus;
  MemoResponse.Lines.Add('All tokens cleared!');
end;

end.