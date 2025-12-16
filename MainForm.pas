unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, REST.Client, REST.Types,
  System.JSON, TokenStorage, Vcl.ExtCtrls;

type
  TFormMain = class(TForm)
    GroupBox1: TGroupBox;
    btnLogin: TButton;
    btnRefreshToken: TButton;
    btnGetProtectedData: TButton;
    btnClearTokens: TButton;
    btnShowTokens: TButton;
    MemoResponse: TMemo;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    edtUsername: TEdit;
    edtPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnRefreshTokenClick(Sender: TObject);
    procedure btnGetProtectedDataClick(Sender: TObject);
    procedure btnClearTokensClick(Sender: TObject);
    procedure btnShowTokensClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    FAccessToken: string;
    FRefreshToken: string;
    FTokenExpiry: TDateTime;
    procedure LoadStoredTokens;
    procedure SaveTokens;
    procedure ClearTokens;
    function IsTokenExpired: Boolean;
    procedure RefreshAccessToken;
    procedure SetAuthorizationHeader;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  System.DateUtils;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  RESTClient1.BaseURL := 'http://localhost:3000';
  LoadStoredTokens;
  Timer1.Interval := 60000; // Check every minute
  Timer1.Enabled := True;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  SaveTokens;
end;

procedure TFormMain.LoadStoredTokens;
begin
  FAccessToken := TTokenStorage.LoadAccessToken;
  FRefreshToken := TTokenStorage.LoadRefreshToken;
  FTokenExpiry := TTokenStorage.LoadTokenExpiry;
  
  if FAccessToken <> '' then
    MemoResponse.Lines.Add('Tokens loaded from storage');
end;

procedure TFormMain.SaveTokens;
begin
  TTokenStorage.SaveAccessToken(FAccessToken);
  TTokenStorage.SaveRefreshToken(FRefreshToken);
  TTokenStorage.SaveTokenExpiry(FTokenExpiry);
end;

procedure TFormMain.ClearTokens;
begin
  FAccessToken := '';
  FRefreshToken := '';
  FTokenExpiry := 0;
  TTokenStorage.ClearTokens;
  MemoResponse.Lines.Add('Tokens cleared');
end;

function TFormMain.IsTokenExpired: Boolean;
begin
  Result := (FTokenExpiry > 0) and (Now >= FTokenExpiry);
end;

procedure TFormMain.SetAuthorizationHeader;
begin
  RESTRequest1.Params.Clear;
  RESTRequest1.AddParameter('Authorization', 'Bearer ' + FAccessToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
end;

procedure TFormMain.btnLoginClick(Sender: TObject);
var
  JSONObj, DataObj: TJSONObject;
  JSONValue: TJSONValue;
begin
  RESTRequest1.Method := TRESTRequestMethod.rmPOST;
  RESTRequest1.Resource := '/auth/login';
  RESTRequest1.Params.Clear;
  
  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('username', edtUsername.Text);
    JSONObj.AddPair('password', edtPassword.Text);
    RESTRequest1.AddBody(JSONObj.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    
    try
      RESTRequest1.Execute;
      
      MemoResponse.Lines.Add('Login Response:');
      MemoResponse.Lines.Add(RESTResponse1.Content);
      
      if RESTResponse1.StatusCode = 200 then
      begin
        JSONValue := TJSONObject.ParseJSONValue(RESTResponse1.Content);
        try
          if JSONValue is TJSONObject then
          begin
            DataObj := JSONValue as TJSONObject;
            FAccessToken := DataObj.GetValue<string>('accessToken');
            FRefreshToken := DataObj.GetValue<string>('refreshToken');
            FTokenExpiry := IncSecond(Now, 3600); // Assuming 1 hour expiry
            SaveTokens;
            MemoResponse.Lines.Add('Tokens saved successfully');
          end;
        finally
          JSONValue.Free;
        end;
      end;
    except
      on E: Exception do
        MemoResponse.Lines.Add('Error: ' + E.Message);
    end;
  finally
    JSONObj.Free;
  end;
end;

procedure TFormMain.RefreshAccessToken;
var
  JSONObj, DataObj: TJSONObject;
  JSONValue: TJSONValue;
begin
  if FRefreshToken = '' then
  begin
    MemoResponse.Lines.Add('No refresh token available');
    Exit;
  end;
  
  RESTRequest1.Method := TRESTRequestMethod.rmPOST;
  RESTRequest1.Resource := '/auth/refresh';
  RESTRequest1.Params.Clear;
  
  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('refreshToken', FRefreshToken);
    RESTRequest1.AddBody(JSONObj.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    
    try
      RESTRequest1.Execute;
      
      if RESTResponse1.StatusCode = 200 then
      begin
        JSONValue := TJSONObject.ParseJSONValue(RESTResponse1.Content);
        try
          if JSONValue is TJSONObject then
          begin
            DataObj := JSONValue as TJSONObject;
            FAccessToken := DataObj.GetValue<string>('accessToken');
            FTokenExpiry := IncSecond(Now, 3600);
            SaveTokens;
            MemoResponse.Lines.Add('Access token refreshed successfully');
          end;
        finally
          JSONValue.Free;
        end;
      end
      else
      begin
        MemoResponse.Lines.Add('Failed to refresh token');
        MemoResponse.Lines.Add(RESTResponse1.Content);
      end;
    except
      on E: Exception do
        MemoResponse.Lines.Add('Error refreshing token: ' + E.Message);
    end;
  finally
    JSONObj.Free;
  end;
end;

procedure TFormMain.btnRefreshTokenClick(Sender: TObject);
begin
  RefreshAccessToken;
end;

procedure TFormMain.btnGetProtectedDataClick(Sender: TObject);
begin
  if FAccessToken = '' then
  begin
    MemoResponse.Lines.Add('Please login first');
    Exit;
  end;
  
  if IsTokenExpired then
  begin
    MemoResponse.Lines.Add('Token expired, refreshing...');
    RefreshAccessToken;
  end;
  
  RESTRequest1.Method := TRESTRequestMethod.rmGET;
  RESTRequest1.Resource := '/protected/data';
  SetAuthorizationHeader;
  
  try
    RESTRequest1.Execute;
    
    MemoResponse.Lines.Add('Protected Data Response:');
    MemoResponse.Lines.Add(RESTResponse1.Content);
    
    if RESTResponse1.StatusCode = 401 then
    begin
      MemoResponse.Lines.Add('Token invalid, attempting refresh...');
      RefreshAccessToken;
    end;
  except
    on E: Exception do
      MemoResponse.Lines.Add('Error: ' + E.Message);
  end;
end;

procedure TFormMain.btnClearTokensClick(Sender: TObject);
begin
  ClearTokens;
end;

procedure TFormMain.btnShowTokensClick(Sender: TObject);
var
  AccessToken, RefreshToken: string;
  Expiry: TDateTime;
  StoragePath: string;
begin
  AccessToken := TTokenStorage.LoadAccessToken;
  RefreshToken := TTokenStorage.LoadRefreshToken;
  Expiry := TTokenStorage.LoadTokenExpiry;
  
  MemoResponse.Lines.Add('==================== STORED TOKENS ====================');
  MemoResponse.Lines.Add('Storage Path: ' + IncludeTrailingPathDelimiter(GetEnvironmentVariable('LOCALAPPDATA')) + 'MyRestApp\');
  MemoResponse.Lines.Add('');
  MemoResponse.Lines.Add('Access Token: ' + AccessToken);
  MemoResponse.Lines.Add('');
  MemoResponse.Lines.Add('Refresh Token: ' + RefreshToken);
  MemoResponse.Lines.Add('');
  if Expiry > 0 then
    MemoResponse.Lines.Add('Expiry: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Expiry))
  else
    MemoResponse.Lines.Add('Expiry: Not set');
  MemoResponse.Lines.Add('');
  MemoResponse.Lines.Add('Safe Mode (DPAPI): ' + BoolToStr(TTokenStorage.IsSafeMode, True));
  MemoResponse.Lines.Add('=======================================================');
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  if IsTokenExpired and (FRefreshToken <> '') then
  begin
    MemoResponse.Lines.Add('Auto-refreshing expired token...');
    RefreshAccessToken;
  end;
end;

end.