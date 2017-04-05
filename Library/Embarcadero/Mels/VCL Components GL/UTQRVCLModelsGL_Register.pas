// *************************************************************************************************
// * ==> UTQRVCLModelsGL_Register -----------------------------------------------------------------*
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
 @abstract(@name registers the components in the RAD studio tool palette.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRVCLModelsGL_Register;

interface

uses System.Classes,
     UTQRVCLSimpleRendererComponentGL,
     UTQRVCLShapeComponentGL,
     UTQRVCLMDLModelComponentGL,
     UTQRVCLMD2ModelComponentGL,
     UTQRVCLMD3ModelComponentGL;

{$REGION 'Documentation'}
{**
 Main register procedure
}
{$ENDREGION}
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
                                TQRVCLMDLModelGL,
                                TQRVCLMD2ModelGL,
                                TQRVCLMD3ModelGL]);
end;
//--------------------------------------------------------------------------------------------------

end.