// *************************************************************************************************
// * ==> Main -------------------------------------------------------------------------------------*
// *************************************************************************************************
// * MIT License - The Mels Library, a free and easy-to-use 3D Models library                      *
// *                                                                                               *
// * Permission is hereby granted, free of charge, to any person obtaining a copy of this software *
// * and associated documentation files (the "Software"), to deal in the Software without          *
// * restriction, including without limitation the rights to use, copy, modify, merge, publish,    *
// * distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the *
// * Software is furnished to do so, subject to the following conditions:                          *
// *                                                                                               *
// * The above copyright notice and this permission notice shall be included in all copies or      *
// * substantial portions of the Software.                                                         *
// *                                                                                               *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING *
// * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND    *
// * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,  *
// * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      *
// * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *
// *************************************************************************************************

{**
 @abstract(@name contains the planetarium demo main form.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit Main;

interface

uses System.Classes,
     System.SysUtils,
     System.Variants,
     Vcl.Graphics,
     Vcl.Imaging.jpeg,
     Vcl.Controls,
     Vcl.ExtCtrls,
     Vcl.StdCtrls,
     Vcl.Forms,
     Vcl.Dialogs,
     UTQRVCLModelComponentGL,
     UTQRVCLShapeComponentGL,
     Winapi.Messages,
     Winapi.Windows;

type
    {**
     Main form
    }
    TMainForm = class(TForm)
        published
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

            procedure tiAnimationTimer(pSender: TObject);

        private
            m_Angle: Single;

        public
            {**
             Constructor
             @param (pOwner Form owner)
            }
            constructor Create(pOwner: TComponent); override;
    end;

var
    MainForm: TMainForm;

implementation
//--------------------------------------------------------------------------------------------------
// Resources
//--------------------------------------------------------------------------------------------------
{$R *.dfm}
//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
constructor TMainForm.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    m_Angle := 0.0;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.tiAnimationTimer(pSender: TObject);
begin
    // calculate next angle
    m_Angle := m_Angle + 0.025;

    // limit angle to 2 * PI value
    if (m_Angle > PI * 2) then
        m_Angle := m_Angle - (PI * 2);

    // update mercury planet
    spMercury.Model.RotationY := m_Angle;
    spMercury.Invalidate;

    // update venus planet
    spVenus.Model.RotationY := m_Angle;
    spVenus.Invalidate;

    // update earth planet
    spEarth.Model.RotationY := m_Angle;
    spEarth.Invalidate;

    // update moon
    spMoon.Model.RotationY := m_Angle;
    spMoon.Invalidate;

    // update mars planet
    spMars.Model.RotationY := m_Angle;
    spMars.Invalidate;

    // update jupiter planet
    spJupiter.Model.RotationY := m_Angle;
    spJupiter.Invalidate;

    // update saturn planet
    spSaturn.Model.RotationY := m_Angle;
    spSaturn.Invalidate;

    // update uranus planet
    spUranus.Model.RotationY := m_Angle;
    spUranus.Invalidate;

    // update neptune planet
    spNeptune.Model.RotationY := m_Angle;
    spNeptune.Invalidate;
end;
//--------------------------------------------------------------------------------------------------

end.
