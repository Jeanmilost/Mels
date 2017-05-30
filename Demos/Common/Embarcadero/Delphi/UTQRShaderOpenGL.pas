// *************************************************************************************************
// * ==> UTQRShaderOpenGL -------------------------------------------------------------------------*
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
 @abstract(@name provides the features to access the OpenGL shader language to use to render models.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRShaderOpenGL;

interface

uses System.Classes,
     System.SysUtils,
     UTQR3D,
     Winapi.OpenGL,
     Winapi.Windows,
     {$IF CompilerVersion <= 25}
         // for compiler until XE4 (not sure until which version), the DelphiGL library is required,
         // because the OpenGL include provided by Embarcadero is incomplete
         DelphiGL.OpenGL,
         DelphiGL.OpenGLext;
     {$ELSE}
         Winapi.OpenGLext;
     {$ENDIF}

type
    {$REGION 'Documentation'}
    {**
     Called when a generic attribute should be associated with a named variable
     @param(pSender Event sender)
     @param(programID Shader program identifier)
     @param(shaderType Shader type)
    }
    {$ENDREGION}
    TQRShaderBindAttributeEvent = procedure(pSender: TObject;
                                          programID: NativeUInt;
                                         shaderType: EQRShaderType) of object;

    {$REGION 'Documentation'}
    {**
     OpenGL shader language to use to render models
    }
    {$ENDREGION}
    TQRShaderOpenGL = class(TQRShader)
        private
            m_ProgramID:        GLuint;
            m_VertexID:         GLuint;
            m_FragmentID:       GLuint;
            m_fOnBindAttribute: TQRShaderBindAttributeEvent;

        protected
            {$REGION 'Documentation'}
            {**
             Compiles shader from file
             @param(fileName Shader file name to compile)
             @param(shaderType Shader type)
             @return(Compiled shader identifier)
            }
            {$ENDREGION}
            function CompileFile(const fileName: TFileName; shaderType: GLenum): GLuint; virtual;

            {$REGION 'Documentation'}
            {**
             Compiles shader
             @param(source Shader source code)
             @param(type Shader type)
             @return(Compiled shader identifier)
            }
            {$ENDREGION}
            function Compile(const source: AnsiString; shaderType: GLenum): GLuint; virtual;

            {$REGION 'Documentation'}
            {**
             Logs the shader error content to compiler console
            }
            {$ENDREGION}
            procedure LogShaderError; virtual;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Creates program
            }
            {$ENDREGION}
            procedure CreateProgram; virtual;

            {$REGION 'Documentation'}
            {**
             Gets shader program identifier
             @return(Shader program identifier)
            }
            {$ENDREGION}
            function GetProgramID: NativeUInt; override;

            {$REGION 'Documentation'}
            {**
             Attaches shader to program from stream
             @param(pStream Stream containing shader)
             @param(type Shader type)
             @return(Compiled shader identifier)
            }
            {$ENDREGION}
            function AttachFile(pStream: TStream;
                             shaderType: EQRShaderType): NativeUInt; overload; override;

            {$REGION 'Documentation'}
            {**
             Attaches shader to program from file
             @param(fileName Shader file)
             @param(type Shader type)
             @return(Compiled shader identifier)
            }
            {$ENDREGION}
            function AttachFile(const fileName: TFileName;
                                    shaderType: EQRShaderType): NativeUInt; overload; override;

            {$REGION 'Documentation'}
            {**
             Attaches shader to program
             @param(source Shader source code)
             @param(type Shader type)
             @return(Compiled shader identifier)
            }
            {$ENDREGION}
            function Attach(const source: UnicodeString;
                              shaderType: EQRShaderType): NativeUInt; override;

            {$REGION 'Documentation'}
            {**
             Links all attached shader and keep program ready to run
             @param(useProgram If @true, program will be used immediately (in case link succeeded))
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Link(useProgram: Boolean): Boolean; override;

            {$REGION 'Documentation'}
            {**
             Uses the program
             @param(use If @true, program will be used, released otherwise)
            }
            {$ENDREGION}
            procedure Use(use: Boolean); override;

            {$REGION 'Documentation'}
            {**
             Converts shader type to OpenGL shader type
             @param(shaderType Shader type to convert)
             @return(Converted OpenGL shader type)
            }
            {$ENDREGION}
            class function ShaderTypeToOpenGLShaderType(shaderType: EQRShaderType): GLenum; static;

            {$REGION 'Documentation'}
            {**
             Converts OpenGL shader type to shader type
             @param(shaderType OpenGL shader type to convert)
             @return(Converted shader type)
            }
            {$ENDREGION}
            class function OpenGLTypeToShaderType(shaderType: GLenum): EQRShaderType; static;

            {$REGION 'Documentation'}
            {**
             Sets OnBindAttribute callback
             @param(fHandler Function handler)
            }
            {$ENDREGION}
            procedure Set_OnBindAttribute(fHandler: TQRShaderBindAttributeEvent); virtual;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRShaderOpenGL
//--------------------------------------------------------------------------------------------------
constructor TQRShaderOpenGL.Create;
begin
    inherited Create;

    m_ProgramID        := 0;
    m_VertexID         := 0;
    m_FragmentID       := 0;
    m_fOnBindAttribute := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRShaderOpenGL.Destroy;
begin
    // delete vertex shader, if needed
    if (m_VertexID <> 0) then
    begin
        glDetachShader(m_ProgramID, m_VertexID);
        glDeleteShader(m_VertexID);
        m_VertexID := 0;
    end;

    // delete fragment shader, if needed
    if (m_FragmentID <> 0) then
    begin
        glDetachShader(m_ProgramID, m_FragmentID);
        glDeleteShader(m_FragmentID);
        m_FragmentID := 0;
    end;

    // delete program, if needed
    if (m_ProgramID <> 0) then
    begin
        glDeleteProgram(m_ProgramID);
        m_ProgramID := 0;
    end;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRShaderOpenGL.CompileFile(const fileName: TFileName; shaderType: GLenum): GLuint;
var
    pShaderFile:   Text;
    text:          UnicodeString;
    shaderContent: UnicodeString;
begin
    // file exists?
    if (not FileExists(fileName)) then
        Exit(0);

    // open shader file
    AssignFile(pShaderFile, fileName);
    Reset(pShaderFile);

    try
        // iterate through file lines
        while not Eof(pShaderFile) do
        begin
            ReadLn(pShaderFile, text);
            shaderContent := shaderContent + text + AnsiChar(#10);
        end;
    finally
        CloseFile(pShaderFile);
    end;

    Result := Compile(AnsiString(shaderContent), shaderType);
end;
//--------------------------------------------------------------------------------------------------
function TQRShaderOpenGL.Compile(const source: AnsiString; shaderType: GLenum): GLuint;
var
    shader:        GLuint;
    shaderStrings: array [0 .. 0] of PGLChar;
    shaderLengths: array [0 .. 0] of GLint;
    compiled:      GLint;
begin
    // create new shader
    shader := glCreateShader(shaderType);

    // get source program as array of chars
    shaderLengths[0] := Length(source);
    shaderStrings[0] := @source[1];

    // compile shader
    glShaderSource(shader, 1, @shaderStrings, @shaderLengths);
    glCompileShader(shader);

    // failed?
    if (shader = 0) then
    begin
        {$IFDEF DEBUG}
            LogShaderError;
        {$IFEND}

        Exit(0);
    end;

    // query program to know if link succeeded
    glGetProgramiv(m_ProgramID, GL_COMPILE_STATUS, @compiled);

    // succeeded?
    if (compiled = 0) then
    begin
        {$IFDEF DEBUG}
            LogShaderError;
        {$IFEND}

        Exit(0);
    end;

    // return shader index
    Result := shader;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRShaderOpenGL.LogShaderError;
{$IFDEF DEBUG}
    var
        logLength: GLint;
        log:       AnsiString;
{$IFEND}
begin
    {$IFDEF DEBUG}
        // log shader compilation errors to compiler console
        glGetShaderiv(m_ProgramID, GL_INFO_LOG_LENGTH, @logLength);

        // nothing to log?
        if (logLength = 0) then
            Exit;

        // log shader error
        SetLength(log, logLength);
        glGetProgramInfoLog(m_ProgramID, 1024, @logLength, @log[1]);
        OutputDebugString(PWideChar(UnicodeString(log)));
    {$IFEND}
end;
//--------------------------------------------------------------------------------------------------
procedure TQRShaderOpenGL.CreateProgram;
begin
    // create new shader program
    m_ProgramID := glCreateProgram;

    // succeeded?
    if (m_ProgramID = 0) then
        raise Exception.Create('Failed to create shader program');
end;
//--------------------------------------------------------------------------------------------------
function TQRShaderOpenGL.GetProgramID: NativeUInt;
begin
    Result := m_ProgramID;
end;
//--------------------------------------------------------------------------------------------------
function TQRShaderOpenGL.AttachFile(pStream: TStream; shaderType: EQRShaderType): NativeUInt;
var
    pStringList: TStringList;
begin
    // no stream to read from?
    if (not Assigned(pStream)) then
        Exit(0);

    // create string list
    pStringList := TStringList.Create;

    try
        // read shader from stream
        pStringList.LoadFromStream(pStream);

        // attach shader file content
        Result := Attach(pStringList.Text, shaderType);
    finally
        pStringList.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRShaderOpenGL.AttachFile(const fileName: TFileName;
                                            shaderType: EQRShaderType): NativeUInt;
var
    shaderID: GLuint;
begin
    // compile shader
    shaderID := CompileFile(fileName, ShaderTypeToOpenGLShaderType(shaderType));

    // succeeded?
    if (shaderID = 0) then
        Exit(0);

    // attach shader to program
    glAttachShader(m_ProgramID, shaderID);

    // do bind attributes?
    if (Assigned(m_fOnBindAttribute)) then
        m_fOnBindAttribute(Self, m_ProgramID, shaderType);

    // search for shader type
    case (shaderType) of
        EQR_ST_Vertex:   m_VertexID   := shaderID;
        EQR_ST_Fragment: m_FragmentID := shaderID;
    else
        raise Exception.Create('Unknown shader type');
    end;

    Result := shaderID;
end;
//--------------------------------------------------------------------------------------------------
function TQRShaderOpenGL.Attach(const source: UnicodeString;
                                      shaderType: EQRShaderType): NativeUInt;
var
    shaderID: GLuint;
begin
    // compile shader
    shaderID := Compile(AnsiString(source), ShaderTypeToOpenGLShaderType(shaderType));

    // succeeded?
    if (shaderID = 0) then
        Exit(0);

    // attach shader to program
    glAttachShader(m_ProgramID, shaderID);

    // do bind attributes?
    if (Assigned(m_fOnBindAttribute)) then
        m_fOnBindAttribute(Self, m_ProgramID, shaderType);

    // search for shader type
    case (shaderType) of
        EQR_ST_Vertex:   m_VertexID   := shaderID;
        EQR_ST_Fragment: m_FragmentID := shaderID;
    else
        raise Exception.Create('Unknown shader type');
    end;

    Result := shaderID;
end;
//--------------------------------------------------------------------------------------------------
function TQRShaderOpenGL.Link(useProgram: Boolean): Boolean;
var
    linked: GLint;
 begin
    // link program
    glLinkProgram(m_ProgramID);

    // query program to know if link succeeded
    glGetProgramiv(m_ProgramID, GL_LINK_STATUS, @linked);

    // succeeded?
    if (linked = 0) then
    begin
        {$IFDEF DEBUG}
            LogShaderError;
        {$IFEND}

        Exit(False);
    end;

    // do use linked program immediately?
    if (useProgram) then
        Use(True);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRShaderOpenGL.Use(use: Boolean);
begin
    // do use program and program exists?
    if (use and (m_ProgramID <> 0)) then
        // bind program
        glUseProgram(m_ProgramID)
    else
        // unbind program
        glUseProgram(0);
end;
//--------------------------------------------------------------------------------------------------
class function TQRShaderOpenGL.ShaderTypeToOpenGLShaderType(shaderType: EQRShaderType): GLenum;
begin
    case (shaderType) of
        EQR_ST_Vertex:   Result := GL_VERTEX_SHADER;
        EQR_ST_Fragment: Result := GL_FRAGMENT_SHADER;
    else
        raise Exception.Create('Unknown shader type');
    end;
end;
//--------------------------------------------------------------------------------------------------
class function TQRShaderOpenGL.OpenGLTypeToShaderType(shaderType: GLenum): EQRShaderType;
begin
    case (shaderType) of
        GL_VERTEX_SHADER:   Result := EQR_ST_Vertex;
        GL_FRAGMENT_SHADER: Result := EQR_ST_Fragment;
    else
        raise Exception.Create('Unknown shader type');
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRShaderOpenGL.Set_OnBindAttribute(fHandler: TQRShaderBindAttributeEvent);
begin
    m_fOnBindAttribute := fHandler;
end;
//--------------------------------------------------------------------------------------------------

end.
