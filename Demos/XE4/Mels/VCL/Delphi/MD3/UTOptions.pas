unit UTOptions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TOptions = class(TForm)
    paPreview: TPanel;
    imPreview: TImage;
    paMain: TPanel;
    gbRenderOptions: TGroupBox;
    ckShowCollisions: TCheckBox;
    ckUseShader: TCheckBox;
    ckFullScreen: TCheckBox;
    ckUseOrthoMatrix: TCheckBox;
    rgCacheOptions: TRadioGroup;
    gbLoadModel: TGroupBox;
    edModelFileName: TEdit;
    btBrowse: TButton;
    gbSelectTeam: TGroupBox;
    rbDefault: TRadioButton;
    rbRed: TRadioButton;
    rbBlue: TRadioButton;
    paButtons: TPanel;
    btOK: TButton;
    btCancel: TButton;
    btQuit: TButton;
    tiDrawPreview: TTimer;
    odOpenDialog: TOpenDialog;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Options: TOptions;

implementation

{$R *.dfm}

end.
