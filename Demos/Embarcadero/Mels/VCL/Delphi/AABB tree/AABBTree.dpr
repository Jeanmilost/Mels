program AABBTree;

uses
  Vcl.Forms,
  Main in 'Main.pas' {MainForm},
  UTQROpenGLHelper in '..\..\..\..\..\Common\Embarcadero\Delphi\UTQROpenGLHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
