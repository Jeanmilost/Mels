{**************************************************************************************************
 * ==> UTQRVCLHelpersGL --------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This unit provides helpers to support some common tasks that OpenGL and VCL      *
 *               don't care.                                                                      *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRVCLHelpersGL;

interface
    // do not include Winapi.OpenGLExt in hpp, because it may generate conflicts in C++ code
    (*$NOINCLUDE Winapi.OpenGLext *)

uses Winapi.OpenGL,
     Winapi.OpenGLext;

type
    {**
    * Some helper functions to manipulate OpenGL in a VCL context
    *}
    TQRVCLOpenGLHelper = Record
        public
            {**
            * Initializes OpenGL
            *@return true on success, otherwise false
            *}
            class function InitializeOpenGL: Boolean; static;
    end;

implementation
//------------------------------------------------------------------------------
// TQRVCLOpenGLHelper
//------------------------------------------------------------------------------
class function TQRVCLOpenGLHelper.InitializeOpenGL: Boolean;
begin
    // is OpenGL Extension already initialized?
    if (Assigned(@glGenRenderbuffers)) then
    begin
        Result := True;
        Exit;
    end;

    // initialize OpenGL extension library
    InitOpenGLext;

    // glGenRenderbuffers is one of the most commonly used function in the OpenGL Extension set. If
    // this function isn't available, then the Mels components cannot work
    Result := Assigned(@glGenRenderbuffers);
end;
//------------------------------------------------------------------------------

end.
