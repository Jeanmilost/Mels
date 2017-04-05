program MD3;

{$R 'Main.res' 'Main.rc'}

uses
  Vcl.Forms,
  Main in 'Main.pas' {MainForm},
  UTOptions in 'UTOptions.pas',
  UTQROpenGLHelper in '..\..\..\..\..\..\Common\Delphi\UTQROpenGLHelper.pas',
  UTQRShaderOpenGL in '..\..\..\..\..\..\Common\Delphi\UTQRShaderOpenGL.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
