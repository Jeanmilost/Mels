unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UTQRGeometry, UTQRCollision, UTQRVCLModelComponentGL, UTQRVCLMD2ModelComponentGL;

type
  TMainForm = class(TForm)
    m2Model: TQRVCLMD2ModelGL;
    procedure m2ModelDetectCollisions(pSender: TObject; const modelMatrix: TQRMatrix4x4;
      pAABBTree: TQRAABBTree);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.m2ModelDetectCollisions(pSender: TObject; const modelMatrix: TQRMatrix4x4;
  pAABBTree: TQRAABBTree);
begin
    //
end;

end.
