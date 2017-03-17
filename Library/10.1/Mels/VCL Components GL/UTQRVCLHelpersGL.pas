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

interface
    // do not include Winapi.OpenGLExt in hpp, because it may generate conflicts in C++ code
    (*$NOINCLUDE Winapi.OpenGLext *)

uses Winapi.OpenGL,
     Winapi.OpenGLext;

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
    // is OpenGL Extension already initialized?
    if (Assigned(@glGenRenderbuffers)) then
        Exit(True);

    // initialize OpenGL extension library
    InitOpenGLext;

    // glGenRenderbuffers is one of the most commonly used function in the OpenGL Extension set. If
    // this function isn't available, then the Mels components cannot work
    Result := Assigned(@glGenRenderbuffers);
end;
//--------------------------------------------------------------------------------------------------

end.
