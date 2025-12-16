program RestApiApp;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {FormMain},
  ApiEndpoints in 'ApiEndpoints.pas',
  TokenStorage in 'TokenStorage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.