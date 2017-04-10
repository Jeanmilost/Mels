program ResourcesInstaller;

uses
  Vcl.Forms,
  Main in 'Main.pas' {MainForm},
  UTQRRadStudioHelper in 'UTQRRadStudioHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
