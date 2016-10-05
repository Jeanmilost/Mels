unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UTQRVCLShapeComponentGL, Vcl.Imaging.pngimage,
  UTQRVCLModelComponentGL;

type
  TForm1 = class(TForm)
    suSurface: TQRVCLSurfaceGL;
    boBox: TQRVCLBoxGL;
    spSphere: TQRVCLSphereGL;
    coCone: TQRVCLConeGL;
    toTorus: TQRVCLTorusGL;
    prParabola: TQRVCLParabolaGL;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
