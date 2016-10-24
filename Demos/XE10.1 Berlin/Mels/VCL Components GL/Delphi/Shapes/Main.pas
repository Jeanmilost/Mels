{**************************************************************************************************
 * ==> Main --------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Shapes demo main form                                                            *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit Main;

interface

uses System.SysUtils,
     System.Variants,
     System.Classes,
     Vcl.Graphics,
     Vcl.Imaging.pngimage,
     Vcl.Controls,
     Vcl.Forms,
     Vcl.Dialogs,
     Winapi.Windows,
     Winapi.Messages,
     UTQRVCLShapeComponentGL,
     UTQRVCLModelComponentGL;

type
    {**
    * Shapes demo
    *}
    TMainForm = class(TForm)
        published
            suSurface: TQRVCLSurfaceGL;
            boBox: TQRVCLBoxGL;
            spSphere: TQRVCLSphereGL;
            coCone: TQRVCLConeGL;
            toTorus: TQRVCLTorusGL;
            prParabola: TQRVCLParabolaGL;
    end;

var
    MainForm: TMainForm;

implementation
//--------------------------------------------------------------------------------------------------
{$R *.dfm}
//--------------------------------------------------------------------------------------------------

end.
