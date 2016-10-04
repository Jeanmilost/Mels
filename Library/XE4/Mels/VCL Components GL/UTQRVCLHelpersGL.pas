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
    // do not include XE7.OpenGLExt in hpp, because it may generate conflicts in C++ code
    (*$NOINCLUDE XE7.OpenGLext *)

// unfortunately the required OpenGL headers does not exist or are incomplete in XE4 and earlier, so
// the DelphiGL component (provided with installation) should be used instead
uses XE7.OpenGL, XE7.OpenGLext;

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
//--------------------------------------------------------------------------------------------------
// TQRVCLOpenGLHelper
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------

end.
