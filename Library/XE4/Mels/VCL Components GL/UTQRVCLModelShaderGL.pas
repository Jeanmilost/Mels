{**************************************************************************************************
 * ==> UTQRVCLModelShaderGL ----------------------------------------------------------------------*
 **************************************************************************************************
 * Description : OpenGL shader language to use to render models                                   *
 * Developer   : Jean-Milost Reymond                                                              *
 **************************************************************************************************}

unit UTQRVCLModelShaderGL;

interface
    // do not include XE7.OpenGLExt in hpp, because it may generate conflicts in C++ code
    (*$NOINCLUDE XE7.OpenGLext *)

uses System.Classes,
     System.SysUtils,
     UTQR3D,
     UTQRLogging,
     // unfortunately the required OpenGL headers does not exist or are incomplete in XE4 and
     // earlier, so the DelphiGL component (provided with installation) should be used instead
     XE7.OpenGL,
     XE7.OpenGLext;

type
    {**
    * Called when a generic attribute should be associated with a named variable
    *@param pSender - event sender
    *@param programID - shader program identifier
    *@param shaderType - shader type
    *}
    TQRGLShaderBindAttributeEvent = procedure(pSender: TObject;
                                            programID: NativeUInt;
                                           shaderType: EQRShaderType) of object;

    {**
    * OpenGL shader language to use to render models
    *}
    TQRVCLModelShaderGL = class(TQRShader)
        private
            m_ProgramID:        GLuint;
            m_VertexID:         GLuint;
            m_FragmentID:       GLuint;
            m_fOnBindAttribute: TQRGLShaderBindAttributeEvent;

        protected
            {**
            * Compiles shader from file
            *@param fileName - shader file name to compile
            *@param shaderType - shader type
            *@return compiled shader identifier
            *}
            function CompileFile(const fileName: TFileName; shaderType: GLenum): GLuint; virtual;

            {**
            * Compiles shader
            *@param source - shader source code
            *@param type - shader type
            *@return compiled shader identifier
            *}
            function Compile(const source: AnsiString; shaderType: GLenum): GLuint; virtual;

            {**
            * Logs the shader error content to compiler console
            *}
            procedure LogShaderError(); virtual;

        public
            {**
            * Constructor
            *}
            constructor Create; override;

            {**
            * Destructor
            *}
            destructor Destroy; override;

            {**
            * Creates program
            *}
            procedure CreateProgram(); virtual;

            {**
            * Gets shader program identifier
            *@return shader program identifier
            *}
            function GetProgramID(): NativeUInt; override;

            {**
            * Attaches shader to program from stream
            *@param pStream - stream containing shader
            *@param type - shader type
            *@return compiled shader identifier
            *}
            function AttachFile(pStream: TStream;
                             shaderType: EQRShaderType): NativeUInt; overload; override;

            {**
            * Attaches shader to program from file
            *@param fileName - shader file
            *@param type - shader type
            *@return compiled shader identifier
            *}
            function AttachFile(const fileName: TFileName;
                                    shaderType: EQRShaderType): NativeUInt; overload; override;

            {**
            * Attaches shader to program
            *@param source - shader source code
            *@param type - shader type
            *@return compiled shader identifier
            *}
            function Attach(const source: UnicodeString;
                              shaderType: EQRShaderType): NativeUInt; override;

            {**
            * Links all attached shader and keep program ready to run
            *@param useProgram - if true, program will be used immediately (in case link succeeded)
            *@return true on success, otherwise false
            *}
            function Link(useProgram: Boolean): Boolean; override;

            {**
            * Uses the program
            *@param use - if true, program will be used, released otherwise
            *}
            procedure Use(use: Boolean); override;

            {**
            * Converts shader type to OpenGL shader type
            *@param shaderType - shader type to convert
            *@return converted OpenGL shader type
            *}
            class function ShaderTypeToOpenGLShaderType(shaderType: EQRShaderType): GLenum; static;

            {**
            * Converts OpenGL shader type to shader type
            *@param shaderType - OpenGL shader type to convert
            *@return converted shader type
            *}
            class function OpenGLTypeToShaderType(shaderType: GLenum): EQRShaderType; static;

            {**
            * Sets OnBindAttribute callback
            *@param fHandler - function handler
            *}
            procedure Set_OnBindAttribute(fHandler: TQRGLShaderBindAttributeEvent); virtual;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRVCLModelShaderGL
//--------------------------------------------------------------------------------------------------
constructor TQRVCLModelShaderGL.Create;
begin
    inherited Create;

    m_ProgramID        := 0;
    m_VertexID         := 0;
    m_FragmentID       := 0;
    m_fOnBindAttribute := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLModelShaderGL.Destroy;
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
function TQRVCLModelShaderGL.CompileFile(const fileName: TFileName; shaderType: GLenum): GLuint;
var
    pShaderFile:   Text;
    text:          UnicodeString;
    shaderContent: UnicodeString;
begin
    // file exists?
    if (not FileExists(fileName)) then
    begin
        Result := 0;
        Exit;
    end;

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
function TQRVCLModelShaderGL.Compile(const source: AnsiString; shaderType: GLenum): GLuint;
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
            LogShaderError();
        {$IFEND}

        Result := 0;
        Exit;
    end;

    // query program to know if link succeeded
    glGetProgramiv(m_ProgramID, GL_COMPILE_STATUS, @compiled);

    // succeeded?
    if (compiled = 0) then
    begin
        {$IFDEF DEBUG}
            LogShaderError();
        {$IFEND}

        Result := 0;
        Exit;
    end;

    // return shader index
    Result := shader;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelShaderGL.LogShaderError();
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
        TQRLogHelper.LogToCompiler(UnicodeString(log));
    {$IFEND}
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelShaderGL.CreateProgram();
begin
    // create new shader program
    m_ProgramID := glCreateProgram();

    // succeeded?
    if (m_ProgramID = 0) then
        raise Exception.Create('Failed to create shader program');
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelShaderGL.GetProgramID(): NativeUInt;
begin
    Result := m_ProgramID;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelShaderGL.AttachFile(pStream: TStream; shaderType: EQRShaderType): NativeUInt;
var
    pStringList: TStringList;
begin
    // no stream to read from?
    if (not Assigned(pStream)) then
    begin
        Result := 0;
        Exit;
    end;

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
function TQRVCLModelShaderGL.AttachFile(const fileName: TFileName;
                                            shaderType: EQRShaderType): NativeUInt;
var
    shaderID: GLuint;
begin
    // compile shader
    shaderID := CompileFile(fileName, ShaderTypeToOpenGLShaderType(shaderType));

    // succeeded?
    if (shaderID = 0) then
    begin
        Result := 0;
        Exit;
    end;

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
function TQRVCLModelShaderGL.Attach(const source: UnicodeString;
                                      shaderType: EQRShaderType): NativeUInt;
var
    shaderID: GLuint;
begin
    // compile shader
    shaderID := Compile(AnsiString(source), ShaderTypeToOpenGLShaderType(shaderType));

    // succeeded?
    if (shaderID = 0) then
    begin
        Result := 0;
        Exit;
    end;

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
function TQRVCLModelShaderGL.Link(useProgram: Boolean): Boolean;
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
            LogShaderError();
        {$IFEND}

        Result := False;
        Exit;
    end;

    // do use linked program immediately?
    if (useProgram) then
        Use(True);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelShaderGL.Use(use: Boolean);
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
class function TQRVCLModelShaderGL.ShaderTypeToOpenGLShaderType(shaderType: EQRShaderType): GLenum;
begin
    case (shaderType) of
        EQR_ST_Vertex:   Result := GL_VERTEX_SHADER;
        EQR_ST_Fragment: Result := GL_FRAGMENT_SHADER;
    else
        raise Exception.Create('Unknown shader type');
    end;
end;
//--------------------------------------------------------------------------------------------------
class function TQRVCLModelShaderGL.OpenGLTypeToShaderType(shaderType: GLenum): EQRShaderType;
begin
    case (shaderType) of
        GL_VERTEX_SHADER:   Result := EQR_ST_Vertex;
        GL_FRAGMENT_SHADER: Result := EQR_ST_Fragment;
    else
        raise Exception.Create('Unknown shader type');
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelShaderGL.Set_OnBindAttribute(fHandler: TQRGLShaderBindAttributeEvent);
begin
    m_fOnBindAttribute := fHandler;
end;
//--------------------------------------------------------------------------------------------------

end.
