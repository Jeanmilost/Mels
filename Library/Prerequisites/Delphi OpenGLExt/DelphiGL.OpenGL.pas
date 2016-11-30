unit DelphiGL.OpenGL;

interface

uses Winapi.OpenGL,
     Winapi.Windows;

const
    GL_VERTEX_ARRAY        = $8074;
    {$EXTERNALSYM GL_VERTEX_ARRAY}
    GL_NORMAL_ARRAY        = $8075;
    {$EXTERNALSYM GL_NORMAL_ARRAY}
    GL_TEXTURE_COORD_ARRAY = $8078;
    {$EXTERNALSYM GL_TEXTURE_COORD_ARRAY}
    GL_COLOR_ARRAY         = $8076;
    {$EXTERNALSYM GL_COLOR_ARRAY}
    GL_RGBA8               = $8058;
    {$EXTERNALSYM GL_RGBA8}

    procedure glBindTexture(target: GLenum; texture: GLuint); stdcall;
    {$EXTERNALSYM glBindTexture}

    procedure glGenTextures (pname: GLenum; params: PGLuint); stdcall;
    {$EXTERNALSYM glGenTextures}

    procedure glEnableClientState (array_: GLenum); stdcall;
    {$EXTERNALSYM glEnableClientState}

    procedure glVertexPointer(size: GLint; type_: GLenum; stride: GLsizei; pointer: Pointer); stdcall;
    {$EXTERNALSYM glVertexPointer}

    procedure glNormalPointer (type_: GLenum; stride: GLsizei; pointer: Pointer); stdcall;
    {$EXTERNALSYM glNormalPointer}

    procedure glTexCoordPointer (size: GLint; type_: GLenum; stride: GLsizei; pointer: Pointer); stdcall;
    {$EXTERNALSYM glTexCoordPointer}

    procedure glColorPointer (size: GLint; type_: GLenum; stride: GLsizei; pointer: Pointer); stdcall;
    {$EXTERNALSYM glColorPointer}

    procedure glDrawArrays (mode: GLenum; first: GLint; count: GLsizei); stdcall;
    {$EXTERNALSYM glDrawArrays}

    procedure glDisableClientState (array_: GLenum); stdcall;
    {$EXTERNALSYM glDisableClientState}

implementation
procedure glBindTexture; external opengl32;
procedure glGenTextures; external opengl32;
procedure glEnableClientState; external opengl32;
procedure glVertexPointer; external opengl32;
procedure glNormalPointer; external opengl32;
procedure glTexCoordPointer; external opengl32;
procedure glColorPointer; external opengl32;
procedure glDrawArrays; external opengl32;
procedure glDisableClientState; external opengl32;

end.
