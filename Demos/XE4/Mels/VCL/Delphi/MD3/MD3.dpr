program MD3;

uses
  Vcl.Forms,
  Main in 'Main.pas' {MainForm},
  UTQROpenGLHelper in '..\..\..\..\..\Common\Delphi\UTQROpenGLHelper.pas',
  UTQRShaderOpenGL in '..\..\..\..\..\Common\Delphi\UTQRShaderOpenGL.pas',
  UTOptions in 'UTOptions.pas' {Options};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
