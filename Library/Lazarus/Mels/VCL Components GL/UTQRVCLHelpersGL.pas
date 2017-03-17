// *************************************************************************************************
// * ==> UTQRVCLHelpersGL -------------------------------------------------------------------------*
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
 @abstract(@name helpers to support some common tasks that OpenGL and VCL don't care.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRVCLHelpersGL;

{$MODE Delphi}

interface

uses Gl,
     GLext;

type
    {$REGION 'Documentation'}
    {**
     Some helper functions to manipulate OpenGL in a VCL context
    }
    {$ENDREGION}
    TQRVCLOpenGLHelper = record
        {$REGION 'Documentation'}
        {**
         Initializes OpenGL
         @return(@true on success, otherwise @false)
        }
        {$ENDREGION}
        class function InitializeOpenGL: Boolean; static;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRVCLOpenGLHelper
//--------------------------------------------------------------------------------------------------
class function TQRVCLOpenGLHelper.InitializeOpenGL: Boolean;
begin
    // initialize OpenGL 4.0 extension library
    Result := Load_GL_VERSION_4_0;

    // failed?
    if (Result) then
       Exit;

    // initialize OpenGL 3.3 extension library
    Result := Load_GL_VERSION_3_3;

    // failed?
    if (Result) then
       Exit;

    // initialize OpenGL 3.2 extension library
    Result := Load_GL_VERSION_3_2;

    // failed?
    if (Result) then
       Exit;

    // initialize OpenGL 3.1 extension library
    Result := Load_GL_VERSION_3_1;

    // failed?
    if (Result) then
       Exit;

    // initialize OpenGL 3.0 extension library
    Result := Load_GL_VERSION_3_0;

    // failed?
    if (Result) then
       Exit;

    // initialize OpenGL 2.1 extension library
    Result := Load_GL_VERSION_2_1;

    // failed?
    if (Result) then
       Exit;

    // nothing succeeded, last hope is OpenGL 2.0. An earlier version will not support the shader
    Result := glext_LoadExtension('GL_version_2_0');
end;
//--------------------------------------------------------------------------------------------------

end.
