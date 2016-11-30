// *************************************************************************************************
// * ==> DelphiGL.OpenGL --------------------------------------------------------------------------*
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
 @abstract(@name is a complement for the missing OpenGL functions in the XE4 and earlier compiler
           versions.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
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
