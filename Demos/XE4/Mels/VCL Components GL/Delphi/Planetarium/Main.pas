unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Imaging.pngimage, UTQRVCLModelComponentGL,
  UTQRVCLShapeComponentGL, Vcl.StdCtrls, Vcl.Imaging.jpeg;

type
  TMainForm = class(TForm)
    sbMain: TScrollBox;
    paMercury: TPanel;
    spMercury: TQRVCLSphereGL;
    tiAnimation: TTimer;
    laMercury: TLabel;
    paMercuryMain: TPanel;
    laMercuryDesc: TLabel;
    paVenus: TPanel;
    spVenus: TQRVCLSphereGL;
    paVenusMain: TPanel;
    laVenus: TLabel;
    laVenusDesc: TLabel;
    paEarth: TPanel;
    spEarth: TQRVCLSphereGL;
    paEarthMain: TPanel;
    laEarth: TLabel;
    laEarthDesc: TLabel;
    paMars: TPanel;
    spMars: TQRVCLSphereGL;
    paMarsMain: TPanel;
    laMain: TLabel;
    laMarsDesc: TLabel;
    paMoon: TPanel;
    spMoon: TQRVCLSphereGL;
    paMoonMain: TPanel;
    laMoon: TLabel;
    laMoonDesc: TLabel;
    paJupiter: TPanel;
    spJupiter: TQRVCLSphereGL;
    paJupiterMain: TPanel;
    laJupiter: TLabel;
    laJupiterDesc: TLabel;
    paSaturn: TPanel;
    spSaturn: TQRVCLSphereGL;
    paSaturnMain: TPanel;
    laSaturn: TLabel;
    laSaturnDesc: TLabel;
    paUranus: TPanel;
    spUranus: TQRVCLSphereGL;
    paUranusMain: TPanel;
    laUranus: TLabel;
    laUranusDesc: TLabel;
    paNeptune: TPanel;
    spNeptune: TQRVCLSphereGL;
    paNeptuneMain: TPanel;
    laNeptune: TLabel;
    laNeptuneDesc: TLabel;
    procedure tiAnimationTimer(Sender: TObject);
  private
    m_Angle: Single;

  public
    constructor Create(pOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

constructor TMainForm.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    m_Angle := 0.0;
end;

procedure TMainForm.tiAnimationTimer(Sender: TObject);
begin
    m_Angle := m_Angle + 0.025;

    if (m_Angle > PI * 2) then
        m_Angle := m_Angle - (PI * 2);

    spMercury.Model.RotationY := m_Angle;
    spMercury.Invalidate;

    spVenus.Model.RotationY := m_Angle;
    spVenus.Invalidate;

    spEarth.Model.RotationY := m_Angle;
    spEarth.Invalidate;

    spMoon.Model.RotationY := m_Angle;
    spMoon.Invalidate;

    spMars.Model.RotationY := m_Angle;
    spMars.Invalidate;

    spJupiter.Model.RotationY := m_Angle;
    spJupiter.Invalidate;

    spSaturn.Model.RotationY := m_Angle;
    spSaturn.Invalidate;

    spUranus.Model.RotationY := m_Angle;
    spUranus.Invalidate;

    spNeptune.Model.RotationY := m_Angle;
    spNeptune.Invalidate;
end;

end.
