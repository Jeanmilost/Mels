{**************************************************************************************************
 * ==> UTQRVCLModelsGL_Register ------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module allows to register components in RAD studio tool palette             *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRVCLModelsGL_Register;

interface

uses System.Classes,
     UTQRVCLSimpleRendererComponentGL,
     UTQRVCLShapeComponentGL,
     UTQRVCLMD2ModelComponentGL,
     UTQRVCLMD3ModelComponentGL;

{**
* Main register procedure
*}
procedure Register;

implementation
//--------------------------------------------------------------------------------------------------
// Main register procedure
//--------------------------------------------------------------------------------------------------
procedure Register;
begin
    // register all components to show in the designer toolbar
    RegisterComponents('Mels', [TQRVCLSimpleRendererGL,
                                TQRVCLSurfaceGL,
                                TQRVCLBoxGL,
                                TQRVCLSphereGL,
                                TQRVCLConeGL,
                                TQRVCLTorusGL,
                                TQRVCLParabolaGL,
                                TQRVCLMD2ModelGL,
                                TQRVCLMD3ModelGL]);

    {REM
    // link texture collection property editor with texture collection
    RegisterPropertyEditor(TypeInfo(TQRPropTextureCollection), nil, '', TQRPropTextureCollectionEditor);

    // link MD2 model editor with model class
    RegisterComponentEditor(TQRMD2Model, TQRMD2ModelEditor);
    }
end;
//--------------------------------------------------------------------------------------------------

end.
