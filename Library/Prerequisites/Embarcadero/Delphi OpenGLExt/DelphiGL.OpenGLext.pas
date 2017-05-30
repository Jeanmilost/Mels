// *************************************************************************************************
// * ==> DelphiGL.OpenGLExt -----------------------------------------------------------------------*
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
 @abstract(@name is a complement for the missing OpenGL extended functions in the XE4 and earlier
           compiler versions.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit DelphiGL.OpenGLext;

interface
    {$IF CompilerVersion > 25}
        // the DelphiGL package was provided to compensate several missing OpenGL functions the Mels
        // library requires when compiled with RAD Studio XE4 and earlier. As these functions were
        // provided in the more recent compiler versions, this package become obsolete and the native
        // Winapi.OpenGL and Winapi.OpenGLext units should be used instead. If this warning appear,
        // then the package is no longer required and should no more be compiled. For convenience,
        // it can also be removed from the project group (NOTE in this case the DelphiGL.dcp item
        // should also be removed from the Mels VCL Components GL package's Requires list)
        {$MESSAGE WARN 'DelphiGL is no longer required for this RAD Studio version and should no more be compiled. This package and his requirements may be removed if desired.'}
    {$ENDIF}

uses Winapi.OpenGL;

const
    GL_TEXTURE0             = $84C0;
    {$EXTERNALSYM GL_TEXTURE0}
    GL_COMPILE_STATUS       = $8B81;
    {$EXTERNALSYM GL_COMPILE_STATUS}
    GL_INFO_LOG_LENGTH      = $8B84;
    {$EXTERNALSYM GL_INFO_LOG_LENGTH}
    GL_LINK_STATUS          = $8B82;
    {$EXTERNALSYM GL_LINK_STATUS}
    GL_VERTEX_SHADER        = $8B31;
    {$EXTERNALSYM GL_VERTEX_SHADER}
    GL_FRAGMENT_SHADER      = $8B30;
    {$EXTERNALSYM GL_FRAGMENT_SHADER}
    GL_FRAMEBUFFER          = $8D40;
    {$EXTERNALSYM GL_FRAMEBUFFER}
    GL_RENDERBUFFER         = $8D41;
    {$EXTERNALSYM GL_RENDERBUFFER}
    GL_COLOR_ATTACHMENT0    = $8CE0;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT0}
    GL_COLOR_ATTACHMENT1    = $8CE1;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT1}
    GL_COLOR_ATTACHMENT2    = $8CE2;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT2}
    GL_COLOR_ATTACHMENT3    = $8CE3;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT3}
    GL_COLOR_ATTACHMENT4    = $8CE4;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT4}
    GL_COLOR_ATTACHMENT5    = $8CE5;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT5}
    GL_COLOR_ATTACHMENT6    = $8CE6;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT6}
    GL_COLOR_ATTACHMENT7    = $8CE7;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT7}
    GL_COLOR_ATTACHMENT8    = $8CE8;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT8}
    GL_COLOR_ATTACHMENT9    = $8CE9;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT9}
    GL_COLOR_ATTACHMENT10   = $8CEA;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT10}
    GL_COLOR_ATTACHMENT11   = $8CEB;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT11}
    GL_COLOR_ATTACHMENT12   = $8CEC;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT12}
    GL_COLOR_ATTACHMENT13   = $8CED;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT13}
    GL_COLOR_ATTACHMENT14   = $8CEE;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT14}
    GL_COLOR_ATTACHMENT15   = $8CEF;
    {$EXTERNALSYM GL_COLOR_ATTACHMENT15}
    GL_DEPTH_ATTACHMENT     = $8D00;
    {$EXTERNALSYM GL_DEPTH_ATTACHMENT}
    GL_STENCIL_ATTACHMENT   = $8D20;
    {$EXTERNALSYM GL_STENCIL_ATTACHMENT}
    GL_FRAMEBUFFER_COMPLETE = $8CD5;
    {$EXTERNALSYM GL_FRAMEBUFFER_COMPLETE}
    GL_DRAW_FRAMEBUFFER     = $8CA9;
    {$EXTERNALSYM GL_DRAW_FRAMEBUFFER}

type
    PGLenum     = ^GLenum;
    {$EXTERNALSYM PGLenum}
    PPGLboolean = ^PGLboolean;
    {$EXTERNALSYM PPGLboolean}
    GLsizeiptr  =  IntPtr;
    {$EXTERNALSYM GLsizeiptr}
    PGLsizeiptr = ^GLsizeiptr;
    GLintptr    =  IntPtr;
    {$EXTERNALSYM GLintptr}
    PGLintptr   = ^GLintptr;
    GLchar      =  Byte;
    {$EXTERNALSYM GLchar}
    PGLchar     =  MarshaledAString;
    PPGLchar    = ^PGLchar;
    GLhalf      =  WORD;
    {$EXTERNALSYM GLhalf}
    PGLhalf     = ^GLhalf;
    GLsync      =  Pointer;
    {$EXTERNALSYM GLsync}
    PGLsync     = ^GLsync;
    GLint64     =  Int64;
    {$EXTERNALSYM GLint64}
    PGLint64    = ^GLint64;
    GLuint64    =  UInt64;
    {$EXTERNALSYM GLuint64}
    PGLuint64   = ^GLuint64;
    GLfixed     =  GLint;
    {$EXTERNALSYM GLfixed}
    PGLfixed    = ^GLfixed;

var
    glGenRenderbuffers: procedure(n: GLsizei; renderbuffers: PGLuint); stdcall;
    glActiveTexture: procedure(texture: GLenum); stdcall;
    glGetUniformLocation: function(program_: GLuint; const name: PGLchar): GLint; stdcall;
    glGetAttribLocation: function(program_: GLuint; const name: PGLchar): GLint; stdcall;
    glLinkProgram: procedure(program_: GLuint); stdcall;
    glShaderSource: procedure(shader: GLuint; count: GLsizei; const string_: PPGLchar; const length: PGLint); stdcall;
    glUseProgram: procedure(program_: GLuint); stdcall;
    glUniform1f: procedure(location: GLint; v0: GLfloat); stdcall;
    glUniform2f: procedure(location: GLint; v0: GLfloat; v1: GLfloat); stdcall;
    glUniform3f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); stdcall;
    glUniform4f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); stdcall;
    glUniform1i: procedure(location: GLint; v0: GLint); stdcall;
    glUniform2i: procedure(location: GLint; v0: GLint; v1: GLint); stdcall;
    glUniform3i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); stdcall;
    glUniform4i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); stdcall;
    glUniform1fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); stdcall;
    glUniform2fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); stdcall;
    glUniform3fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); stdcall;
    glUniform4fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); stdcall;
    glUniform1iv: procedure(location: GLint; count: GLsizei; const value: PGLint); stdcall;
    glUniform2iv: procedure(location: GLint; count: GLsizei; const value: PGLint); stdcall;
    glUniform3iv: procedure(location: GLint; count: GLsizei; const value: PGLint); stdcall;
    glUniform4iv: procedure(location: GLint; count: GLsizei; const value: PGLint); stdcall;
    glUniformMatrix2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); stdcall;
    glUniformMatrix3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); stdcall;
    glUniformMatrix4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); stdcall;
    glDisableVertexAttribArray: procedure(index: GLuint); stdcall;
    glEnableVertexAttribArray: procedure(index: GLuint); stdcall;
    glVertexAttribPointer: procedure(index: GLuint; size: GLint; type_: GLenum; normalized: GLboolean; stride: GLsizei; const pointer: Pointer); stdcall;
    glCreateProgram: function: GLuint; stdcall;
    glCreateShader: function(type_: GLenum): GLuint; stdcall;
    glDeleteProgram: procedure(program_: GLuint); stdcall;
    glDeleteShader: procedure(shader: GLuint); stdcall;
    glDetachShader: procedure(program_: GLuint; shader: GLuint); stdcall;
    glCompileShader: procedure(shader: GLuint); stdcall;
    glGetProgramiv: procedure(program_: GLuint; pname: GLenum; params: PGLint); stdcall;
    glGetShaderiv: procedure(shader: GLuint; pname: GLenum; params: PGLint); stdcall;
    glGetProgramInfoLog: procedure(program_: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: PGLchar); stdcall;
    glAttachShader: procedure(program_: GLuint; shader: GLuint); stdcall;
    glBindFramebuffer: procedure(target: GLenum; framebuffer: GLuint); stdcall;
    glDeleteFramebuffers: procedure(n: GLsizei; const framebuffers: PGLuint); stdcall;
    glGenFramebuffers: procedure(n: GLsizei; framebuffers: PGLuint); stdcall;
    glBindRenderbuffer: procedure(target: GLenum; renderbuffer: GLuint); stdcall;
    glDeleteRenderbuffers: procedure(n: GLsizei; const renderbuffers: PGLuint); stdcall;
    glRenderbufferStorage: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); stdcall;
    glFramebufferRenderbuffer: procedure(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); stdcall;
    glCheckFramebufferStatus: function(target: GLenum): GLenum; stdcall;

    procedure InitOpenGLext;

implementation
procedure InitOpenGLext;
begin
    glGenRenderbuffers         := wglGetProcAddress('glGenRenderbuffers');
    glActiveTexture            := wglGetProcAddress('glActiveTexture');
    glGetUniformLocation       := wglGetProcAddress('glGetUniformLocation');
    glGetAttribLocation        := wglGetProcAddress('glGetAttribLocation');
    glLinkProgram              := wglGetProcAddress('glLinkProgram');
    glShaderSource             := wglGetProcAddress('glShaderSource');
    glUseProgram               := wglGetProcAddress('glUseProgram');
    glUniform1f                := wglGetProcAddress('glUniform1f');
    glUniform2f                := wglGetProcAddress('glUniform2f');
    glUniform3f                := wglGetProcAddress('glUniform3f');
    glUniform4f                := wglGetProcAddress('glUniform4f');
    glUniform1i                := wglGetProcAddress('glUniform1i');
    glUniform2i                := wglGetProcAddress('glUniform2i');
    glUniform3i                := wglGetProcAddress('glUniform3i');
    glUniform4i                := wglGetProcAddress('glUniform4i');
    glUniform1fv               := wglGetProcAddress('glUniform1fv');
    glUniform2fv               := wglGetProcAddress('glUniform2fv');
    glUniform3fv               := wglGetProcAddress('glUniform3fv');
    glUniform4fv               := wglGetProcAddress('glUniform4fv');
    glUniform1iv               := wglGetProcAddress('glUniform1iv');
    glUniform2iv               := wglGetProcAddress('glUniform2iv');
    glUniform3iv               := wglGetProcAddress('glUniform3iv');
    glUniform4iv               := wglGetProcAddress('glUniform4iv');
    glUniformMatrix2fv         := wglGetProcAddress('glUniformMatrix2fv');
    glUniformMatrix3fv         := wglGetProcAddress('glUniformMatrix3fv');
    glUniformMatrix4fv         := wglGetProcAddress('glUniformMatrix4fv');
    glDisableVertexAttribArray := wglGetProcAddress('glDisableVertexAttribArray');
    glEnableVertexAttribArray  := wglGetProcAddress('glEnableVertexAttribArray');
    glVertexAttribPointer      := wglGetProcAddress('glVertexAttribPointer');
    glCreateProgram            := wglGetProcAddress('glCreateProgram');
    glCreateShader             := wglGetProcAddress('glCreateShader');
    glDeleteProgram            := wglGetProcAddress('glDeleteProgram');
    glDeleteShader             := wglGetProcAddress('glDeleteShader');
    glDetachShader             := wglGetProcAddress('glDetachShader');
    glCompileShader            := wglGetProcAddress('glCompileShader');
    glGetProgramiv             := wglGetProcAddress('glGetProgramiv');
    glGetShaderiv              := wglGetProcAddress('glGetShaderiv');
    glGetProgramInfoLog        := wglGetProcAddress('glGetProgramInfoLog');
    glAttachShader             := wglGetProcAddress('glAttachShader');
    glBindFramebuffer          := wglGetProcAddress('glBindFramebuffer');
    glDeleteFramebuffers       := wglGetProcAddress('glDeleteFramebuffers');
    glGenFramebuffers          := wglGetProcAddress('glGenFramebuffers');
    glBindRenderbuffer         := wglGetProcAddress('glBindRenderbuffer');
    glDeleteRenderbuffers      := wglGetProcAddress('glDeleteRenderbuffers');
    glRenderbufferStorage      := wglGetProcAddress('glRenderbufferStorage');
    glFramebufferRenderbuffer  := wglGetProcAddress('glFramebufferRenderbuffer');
    glCheckFramebufferStatus   := wglGetProcAddress('glCheckFramebufferStatus');
end;

end.
