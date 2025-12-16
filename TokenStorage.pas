unit TokenStorage;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles;

type
  TTokenStorage = class
  private
    class var FSafeMode: Boolean;
    class var FAccessToken: string;
    class var FRefreshToken: string;
    class var FTokenExpiry: TDateTime;
    
    class function ProtectData(const Data: string): TBytes; static;
    class function UnprotectData(const Data: TBytes): string; static;
    class function GetStoragePath: string; static;
  public
    class constructor Create;
    class procedure Initialize;
    
    class procedure SaveAccessToken(const Token: string); static;
    class procedure SaveRefreshToken(const Token: string); static;
    class procedure SaveTokenExpiry(const Expiry: TDateTime); static;
    
    class function LoadAccessToken: string; static;
    class function LoadRefreshToken: string; static;
    class function LoadTokenExpiry: TDateTime; static;
    
    class procedure ClearAllTokens; static;
    class function IsSafeMode: Boolean; static;
  end;

implementation

uses
  Winapi.Windows, Winapi.Wincrypt, System.NetEncoding, 
  System.DateUtils, System.IOUtils, ApiEndpoints;

class constructor TTokenStorage.Create;
begin
  FSafeMode := False;
  FAccessToken := '';
  FRefreshToken := '';
  FTokenExpiry := 0;
end;

class procedure TTokenStorage.Initialize;
var
  IniFile: TIniFile;
  ConfigPath: string;
begin
  ConfigPath := ExtractFilePath(ParamStr(0)) + CONFIG_FILE;
  
  if not FileExists(ConfigPath) then
  begin
    IniFile := TIniFile.Create(ConfigPath);
    try
      IniFile.WriteBool(CONFIG_SECTION, CONFIG_SAFE_TOKENS, True);
      FSafeMode := True;
    finally
      IniFile.Free;
    end;
  end
  else
  begin
    IniFile := TIniFile.Create(ConfigPath);
    try
      FSafeMode := IniFile.ReadBool(CONFIG_SECTION, CONFIG_SAFE_TOKENS, True);
    finally
      IniFile.Free;
    end;
  end;
  
  if FSafeMode then
  begin
    FAccessToken := LoadAccessToken;
    FRefreshToken := LoadRefreshToken;
    FTokenExpiry := LoadTokenExpiry;
  end;
end;

class function TTokenStorage.GetStoragePath: string;
begin
  Result := IncludeTrailingPathDelimiter(
    GetEnvironmentVariable('LOCALAPPDATA')) + 'MyRestApp\\';
  ForceDirectories(Result);
end;

class function TTokenStorage.ProtectData(const Data: string): TBytes;
var
  DataIn, DataOut: DATA_BLOB;
  Bytes: TBytes;
begin
  if Data = '' then
    Exit(nil);
    
  Bytes := TEncoding.UTF8.GetBytes(Data);
  DataIn.cbData := Length(Bytes);
  DataIn.pbData := @Bytes[0];
  
  if CryptProtectData(@DataIn, nil, nil, nil, nil, 
     CRYPTPROTECT_UI_FORBIDDEN, @DataOut) then
  begin
    SetLength(Result, DataOut.cbData);
    Move(DataOut.pbData^, Result[0], DataOut.cbData);
    LocalFree(HLOCAL(DataOut.pbData));
  end
  else
    raise Exception.Create('Failed to protect data: ' + SysErrorMessage(GetLastError));
end;

class function TTokenStorage.UnprotectData(const Data: TBytes): string;
var
  DataIn, DataOut: DATA_BLOB;
  Bytes: TBytes;
begin
  if Length(Data) = 0 then
    Exit('');
    
  DataIn.cbData := Length(Data);
  DataIn.pbData := @Data[0];
  
  if CryptUnprotectData(@DataIn, nil, nil, nil, nil, 
     CRYPTPROTECT_UI_FORBIDDEN, @DataOut) then
  begin
    SetLength(Bytes, DataOut.cbData);
    Move(DataOut.pbData^, Bytes[0], DataOut.cbData);
    Result := TEncoding.UTF8.GetString(Bytes);
    LocalFree(HLOCAL(DataOut.pbData));
  end
  else
    Result := '';
end;

class procedure TTokenStorage.SaveAccessToken(const Token: string);
var
  ProtectedData: TBytes;
  FileStream: TFileStream;
  FileName: string;
begin
  FAccessToken := Token;
  
  if FSafeMode then
  begin
    ProtectedData := ProtectData(Token);
    FileName := GetStoragePath + TOKEN_ACCESS + '.dat';
    
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      if Length(ProtectedData) > 0 then
        FileStream.WriteBuffer(ProtectedData[0], Length(ProtectedData));
    finally
      FileStream.Free;
    end;
  end;
end;

class procedure TTokenStorage.SaveRefreshToken(const Token: string);
var
  ProtectedData: TBytes;
  FileStream: TFileStream;
  FileName: string;
begin
  FRefreshToken := Token;
  
  if FSafeMode then
  begin
    ProtectedData := ProtectData(Token);
    FileName := GetStoragePath + TOKEN_REFRESH + '.dat';
    
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      if Length(ProtectedData) > 0 then
        FileStream.WriteBuffer(ProtectedData[0], Length(ProtectedData));
    finally
      FileStream.Free;
    end;
  end;
end;

class procedure TTokenStorage.SaveTokenExpiry(const Expiry: TDateTime);
var
  ProtectedData: TBytes;
  FileStream: TFileStream;
  FileName: string;
  ExpiryStr: string;
begin
  FTokenExpiry := Expiry;
  
  if FSafeMode then
  begin
    ExpiryStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', Expiry);
    ProtectedData := ProtectData(ExpiryStr);
    FileName := GetStoragePath + TOKEN_EXPIRY + '.dat';
    
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      if Length(ProtectedData) > 0 then
        FileStream.WriteBuffer(ProtectedData[0], Length(ProtectedData));
    finally
      FileStream.Free;
    end;
  end;
end;

class function TTokenStorage.LoadAccessToken: string;
var
  ProtectedData: TBytes;
  FileStream: TFileStream;
  FileName: string;
begin
  if FSafeMode then
  begin
    FileName := GetStoragePath + TOKEN_ACCESS + '.dat';
    
    if not FileExists(FileName) then
      Exit('');
      
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      if FileStream.Size > 0 then
      begin
        SetLength(ProtectedData, FileStream.Size);
        FileStream.ReadBuffer(ProtectedData[0], FileStream.Size);
        Result := UnprotectData(ProtectedData);
      end
      else
        Result := '';
    finally
      FileStream.Free;
    end;
    
    FAccessToken := Result;
  end
  else
    Result := FAccessToken;
end;

class function TTokenStorage.LoadRefreshToken: string;
var
  ProtectedData: TBytes;
  FileStream: TFileStream;
  FileName: string;
begin
  if FSafeMode then
  begin
    FileName := GetStoragePath + TOKEN_REFRESH + '.dat';
    
    if not FileExists(FileName) then
      Exit('');
      
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      if FileStream.Size > 0 then
      begin
        SetLength(ProtectedData, FileStream.Size);
        FileStream.ReadBuffer(ProtectedData[0], FileStream.Size);
        Result := UnprotectData(ProtectedData);
      end
      else
        Result := '';
    finally
      FileStream.Free;
    end;
    
    FRefreshToken := Result;
  end
  else
    Result := FRefreshToken;
end;

class function TTokenStorage.LoadTokenExpiry: TDateTime;
var
  ProtectedData: TBytes;
  FileStream: TFileStream;
  FileName: string;
  ExpiryStr: string;
begin
  if FSafeMode then
  begin
    FileName := GetStoragePath + TOKEN_EXPIRY + '.dat';
    
    if not FileExists(FileName) then
      Exit(0);
      
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      if FileStream.Size > 0 then
      begin
        SetLength(ProtectedData, FileStream.Size);
        FileStream.ReadBuffer(ProtectedData[0], FileStream.Size);
        ExpiryStr := UnprotectData(ProtectedData);
        
        if ExpiryStr <> '' then
          Result := StrToDateTimeDef(ExpiryStr, 0)
        else
          Result := 0;
      end
      else
        Result := 0;
    finally
      FileStream.Free;
    end;
    
    FTokenExpiry := Result;
  end
  else
    Result := FTokenExpiry;
end;

class procedure TTokenStorage.ClearAllTokens;
var
  Path: string;
begin
  FAccessToken := '';
  FRefreshToken := '';
  FTokenExpiry := 0;
  
  if FSafeMode then
  begin
    Path := GetStoragePath;
    if DirectoryExists(Path) then
      TDirectory.Delete(Path, True);
  end;
end;

class function TTokenStorage.IsSafeMode: Boolean;
begin
  Result := FSafeMode;
end;

end.