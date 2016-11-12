// *************************************************************************************************
// * ==> QR_OpenGLHelper --------------------------------------------------------------------------*
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

#include "QR_OpenGLHelper.h"

// std
#include <string>

// Mels library
#include <UTQR3D.hpp>

// engine
#include "QR_MathsHelper.h"

#ifdef USE_SHADER
    #pragma link "glewSL.lib"
#endif

//--------------------------------------------------------------------------------------------------
// Global defines
//--------------------------------------------------------------------------------------------------
#define GL_CLAMP_TO_EDGE 0x812F
//--------------------------------------------------------------------------------------------------
// QR_OpenGLHelper
//--------------------------------------------------------------------------------------------------
QR_OpenGLHelper::QR_OpenGLHelper()
{}
//--------------------------------------------------------------------------------------------------
QR_OpenGLHelper::~QR_OpenGLHelper()
{}
//--------------------------------------------------------------------------------------------------
bool QR_OpenGLHelper::EnableOpenGL(HWND hWnd, HDC& hDC, HGLRC& hRC)
{
    // no window handle?
    if (!hWnd)
        return false;

    // get the device context (DC)
    hDC = ::GetDC(hWnd);

    // failed to get device context?
    if (!hDC)
        return false;

    // configure pixel format
    if (!SetPixelFormat(hDC))
        return false;

    // create OpenGL render context
    hRC = wglCreateContext(hDC);

    // succeeded?
    if (!hRC)
        return false;

    // enable OpenGL render context
    if (!wglMakeCurrent(hDC, hRC))
        return false;

    return true;
}
//--------------------------------------------------------------------------------------------------
void QR_OpenGLHelper::DisableOpenGL(HWND hWnd, HDC hDC, HGLRC hRC)
{
    // disable and delete OpenGL context
    if (hRC)
    {
        wglMakeCurrent(NULL, NULL);
        wglDeleteContext(hRC);
    }

    // delete device context
    if (hWnd && hDC)
        ::ReleaseDC(hWnd, hDC);
}
//--------------------------------------------------------------------------------------------------
bool QR_OpenGLHelper::SetPixelFormat(HDC hDC)
{
    // no device context?
    if (!hDC)
        return false;

    ::PIXELFORMATDESCRIPTOR pfd =
    {
        sizeof(::PIXELFORMATDESCRIPTOR),
        1,
        PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER,
        PFD_TYPE_RGBA,
        24,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        32,
        0,
        0,
        PFD_MAIN_PLANE,
        0,
        0,
        0,
    };

    // get best available pixel format
    const int pixelFormat = ::ChoosePixelFormat(hDC, &pfd);

    // set pixel format to use
    return ::SetPixelFormat(hDC, pixelFormat, &pfd);
}
//--------------------------------------------------------------------------------------------------
void QR_OpenGLHelper::CreateViewport(int clientWidth, int clientHeight, bool createPerspective)
{
    // invalid width?
    if (!clientWidth)
        clientWidth = 1;

    // invalid height?
    if (!clientHeight)
        clientHeight = 1;

    // set viewport
    glViewport(0, 0, clientWidth, clientHeight);

    if (!createPerspective)
        return;

    // load projection matrix and initialize it
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    // apply perspective correction
    GLfloat aspect = (GLfloat)clientWidth / (GLfloat)clientHeight;
    gluPerspective(45.0f, aspect, 0.1f, 10000.0f);

    // load model view matrix and initialize it
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
}
//--------------------------------------------------------------------------------------------------
TQRMatrix4x4 QR_OpenGLHelper::GetOrtho(float left,
                                       float right,
                                       float bottom,
                                       float top,
                                       float zNear,
                                       float zFar)
{
    // OpenGL specifications                                    can be rewritten as
    // |   2/(r-l)       0             0            0  |        |  2/(r-l)      0            0            0  |
    // |   0             2/(t-b)       0            0  |   =>   |  0            2/(t-b)      0            0  |
    // |   0             0            -2/(f-n)      0  |        |  0            0            2/(n-f)      0  |
    // |  -(r+l)/(r-l)  -(t+b)/(t-b)  -(f+n)/(f-n)  1  |        |  (r+l)/(l-r)  (t+b)/(b-t)  (f+n)/(n-f)  1  |

    // are input values out of bounds?
    if ((left == right) || (bottom == top) || (zNear == zFar))
        throw "Incorrect input values - cannot create orthogonal matrix";

    // calculate matrix component values
    const float prl = right  + left;
    const float mrl = right  - left;
    const float mlr = left   - right;
    const float ptb = top    + bottom;
    const float mtb = top    - bottom;
    const float mbt = bottom - top;
    const float pfn = zFar   + zNear;
    const float mnf = zNear  - zFar;

    // build matrix
    return TQRMatrix4x4(2.0 / mrl, 0.0,       0.0,       0.0,
                        0.0,       2.0 / mtb, 0.0,       0.0,
                        0.0,       0.0,       2.0 / mnf, 0.0,
                        prl / mlr, ptb / mbt, pfn / mnf, 1.0);
}
//--------------------------------------------------------------------------------------------------
TQRMatrix4x4 QR_OpenGLHelper::GetFrustum(float left,
                                         float right,
                                         float bottom,
                                         float top,
                                         float zNear,
                                         float zFar)
{
    // OpenGL specifications                                   can be rewritten as
    // |  2n/(r-l)     0             0             0  |        |  2n/(r-l)     0            0             0  |
    // |  0            2n/(t-b)      0             0  |   =>   |  0            2n/(t-b)     0             0  |
    // |  (r+l)/(r-l)  (t+b)/(t-b)  -(f+n)/(f-n)  -1  |        |  (r+l)/(r-l)  (t+b)/(t-b)  (f+n)/(n-f)  -1  |
    // |  0            0            -2fn/(f-n)     0  |        |  0            0            2fn/(n-f)     0  |

    // are input values out of bounds?
    if ((zNear <= 0.0f) || (zFar <= 0.0f) || (left == right) || (bottom == top) || (zNear == zFar))
        throw "Incorrect input values - cannot create frustum matrix";

    // calculate matrix component values
    const float x2n  = 2.0f  * zNear;
    const float x2nf = 2.0f  * zNear * zFar;
    const float pfn  = zFar  + zNear;
    const float mnf  = zNear - zFar;
    const float prl  = right + left;
    const float mrl  = right - left;
    const float ptb  = top   + bottom;
    const float mtb  = top   - bottom;

    // build matrix
    return TQRMatrix4x4(x2n / mrl, 0.0,       0.0,         0.0,
                        0.0,       x2n / mtb, 0.0,         0.0,
                        prl / mrl, ptb / mtb, pfn  / mnf, -1.0,
                        0.0,       0.0,       x2nf / mnf,  0.0);
}
//--------------------------------------------------------------------------------------------------
TQRMatrix4x4 QR_OpenGLHelper::GetProjection(float fov,
                                            float width,
                                            float height,
                                            float zNear,
                                            float zFar)
{
    // width or height out of bounds?
    if ((width == 0.0f) || (height == 0.0f))
        throw "Invalid width or height";

    // configure matrix values to use
    const float aspect =  width / height;
    const float top    =  0.5f * tan((fov * M_PI) / 360.0f);
    const float bottom = -top;
    const float right  =  aspect * top;
    const float left   = -right;

    // build and return camera matrix
    return GetFrustum(left, right, bottom, top, zNear, zFar);
}
//--------------------------------------------------------------------------------------------------
TQRMatrix4x4 QR_OpenGLHelper::LookAtLH(TQRVector3D& position,
                                       TQRVector3D& direction,
                                       TQRVector3D& up)
{
    // compute per axis transformations
    TQRVector3D zAxis = direction.Sub(position).Normalize();
    TQRVector3D xAxis = up.Cross(zAxis).Normalize();
    TQRVector3D yAxis = zAxis.Cross(xAxis);

    // create look at matrix, translate eye position
    return TQRMatrix4x4( xAxis.X,              yAxis.X,              zAxis.X,             0.0f,
                         xAxis.Y,              yAxis.Y,              zAxis.Y,             0.0f,
                         xAxis.Z,              yAxis.Z,              zAxis.Z,             0.0f,
                        -xAxis.Dot(position), -yAxis.Dot(position), -zAxis.Dot(position), 1.0f);
}
//--------------------------------------------------------------------------------------------------
TQRMatrix4x4 QR_OpenGLHelper::LookAtRH(TQRVector3D& position,
                                       TQRVector3D& direction,
                                       TQRVector3D& up)
{
    // compute per axis transformations
    TQRVector3D zAxis = direction.Sub(position).Normalize();
    TQRVector3D xAxis = up.Cross(zAxis).Normalize();
    TQRVector3D yAxis = zAxis.Cross(xAxis);

    // create look at matrix, translate eye position
    return TQRMatrix4x4(xAxis.X,             yAxis.X,             zAxis.X,             0.0f,
                        xAxis.Y,             yAxis.Y,             zAxis.Y,             0.0f,
                        xAxis.Z,             yAxis.Z,             zAxis.Z,             0.0f,
                        xAxis.Dot(position), yAxis.Dot(position), zAxis.Dot(position), 1.0f);
}
//--------------------------------------------------------------------------------------------------
TQRVector3D QR_OpenGLHelper::MousePosToGLPoint(HWND hWnd, TQRRect& viewRect)
{
    // no window or control?
    if (!hWnd)
        return TQRVector3D();

    ::POINT p;

    // get mouse position
    ::GetCursorPos(&p);

    // convert to window or control client coordinates
    if (!::ScreenToClient(hWnd, &p))
        return TQRVector3D();

    const float mouseX = p.x;
    const float mouseY = p.y;

    ::RECT clientRect;

    // get window or control client rect
    ::GetClientRect(hWnd, &clientRect);

    // calculate client width and height
    const float clientWidth  = (clientRect.right  - clientRect.left);
    const float clientHeight = (clientRect.bottom - clientRect.top);

    // invalid client width or height?
    if (!clientWidth || !clientHeight)
        return TQRVector3D();

    // convert mouse position to OpenGL position
    return TQRVector3D(viewRect.Min->X + ((mouseX * viewRect.Width)  / clientWidth),
                       viewRect.Min->Y - ((mouseY * viewRect.Height) / clientHeight),
                       0.0f);
}
//--------------------------------------------------------------------------------------------------
void QR_OpenGLHelper::GetBitmapFromOpenGL(TBitmap* pBitmap)
{
    // no bitmap?
    if (!pBitmap)
        return;

    GLint dimensions[4];

    // get viewport dimensions
    glGetIntegerv(GL_VIEWPORT, dimensions);

    TRGBQuad* pRGBBits = NULL;

    try
    {
        // create bits to contain bitmap
        pRGBBits = new TRGBQuad[dimensions[2] * dimensions[3] * 4];

        // flush OpenGL
        glFinish();
        glPixelStorei(GL_PACK_ALIGNMENT,   4);
        glPixelStorei(GL_PACK_ROW_LENGTH,  0);
        glPixelStorei(GL_PACK_SKIP_ROWS,   0);
        glPixelStorei(GL_PACK_SKIP_PIXELS, 0);

        // get pixels from last OpenGL rendering
        glReadPixels(0, 0, dimensions[2], dimensions[3], GL_RGBA, GL_UNSIGNED_BYTE, pRGBBits);

        // configure destination bitmap
        pBitmap->PixelFormat = pf32bit;
        pBitmap->SetSize(dimensions[2], dimensions[3]);

        // configure bitmap header
        TBitmapInfo header;
        header.bmiHeader.biSize        = sizeof(TBitmapInfoHeader);
        header.bmiHeader.biWidth       = dimensions[2];
        header.bmiHeader.biHeight      = dimensions[3];
        header.bmiHeader.biPlanes      = 1;
        header.bmiHeader.biBitCount    = 32;
        header.bmiHeader.biCompression = BI_RGB;
        header.bmiHeader.biSizeImage   = dimensions[2] * dimensions[3] * 4;

        TRGBQuad* pPixel = pRGBBits;

        // swap red and blue in bitmap
        for (GLint x = 0; x < dimensions[2]; ++x)
            for (GLint y = 0; y < dimensions[3]; ++y)
            {
                // swap red and blue in pixel
                std::swap(pPixel->rgbRed, pPixel->rgbBlue);
                ++pPixel;
            }

        // copy bitmap content from OpenGL rendered surface to destination
        SetDIBits(pBitmap->Canvas->Handle,
                  pBitmap->Handle,
                  0,
                  dimensions[3],
                  pRGBBits,
                  &header,
                  DIB_RGB_COLORS);
    }
    __finally
    {
        if (pRGBBits)
            delete[] pRGBBits;
    }
}
//--------------------------------------------------------------------------------------------------
bool QR_OpenGLHelper::BytesFromBitmap(TBitmap* pBitmap, BYTE*& pPixels, bool flipY, bool bgr)
{
    // no bitmap?
    if (!pBitmap)
        return false;

    // is bitmap empty?
    if ((pBitmap->Width <= 0) || (pBitmap->Height <= 0))
        return false;

    // get bitmap and pixel size
    const std::size_t width     = pBitmap->Width;
    const std::size_t height    = pBitmap->Height;
    const std::size_t pixelSize = (pBitmap->PixelFormat == pf32bit) ? sizeof(TRGBQuad) : sizeof(TRGBTriple);

    // calculate line size
    const std::size_t lineSize = width * pixelSize;

    // create pixels buffer
    pPixels = new BYTE[height * lineSize];

    // iterate through bitmap lines
    for (std::size_t y = 0; y < height; ++y)
    {
        // calculate next offset
        const std::size_t offset = flipY ? ((height - 1) - y) * lineSize : y * lineSize;

        // is 24 or 32 bit bitmap?
        if (pBitmap->PixelFormat == pf24bit)
        {
            // get pixels line from bitmap
            TRGBTriple* pLineRGB = reinterpret_cast<TRGBTriple*>(pBitmap->ScanLine[y]);

            // do swap pixels?
            if (bgr)
                // memory copy 24 bit pixels line, as pixels are already in RGB format
                std::memcpy(&pPixels[offset], pLineRGB, lineSize);
            else
                // iterate through line pixels
                for (std::size_t x = 0; x < width; ++x)
                {
                    // calculate next pixel offset
                    const std::size_t offsetX = offset + (x * pixelSize);

                    // copy and swap pixel
                    pPixels[offsetX]     = pLineRGB[x].rgbtRed;
                    pPixels[offsetX + 1] = pLineRGB[x].rgbtGreen;
                    pPixels[offsetX + 2] = pLineRGB[x].rgbtBlue;
                }
        }
        else
        {
            // get pixels line from bitmap
            TRGBQuad* pLineRGBA = reinterpret_cast<TRGBQuad*>(pBitmap->ScanLine[y]);

            // do swap pixels?
            if (bgr)
                // memory copy 32 bit pixels line, as pixels are already in RGB format
                std::memcpy(&pPixels[offset], pLineRGBA, lineSize);
            else
                // iterate through line pixels
                for (std::size_t x = 0; x < width; ++x)
                {
                    // calculate next pixel offset
                    const std::size_t offsetX = offset + (x * pixelSize);

                    // copy and swap pixel
                    pPixels[offsetX]     = pLineRGBA[x].rgbRed;
                    pPixels[offsetX + 1] = pLineRGBA[x].rgbGreen;
                    pPixels[offsetX + 2] = pLineRGBA[x].rgbBlue;
                    pPixels[offsetX + 3] = pLineRGBA[x].rgbReserved;
                }
        }
    }

    return true;
}
//--------------------------------------------------------------------------------------------------
int QR_OpenGLHelper::CreateTexture(WORD   width,
                                   WORD   height,
                                   WORD   format,
                                   void*  pPixels,
                                   GLuint minFilter,
                                   GLuint magFilter,
                                   GLuint targetID)
{
    GLuint texture;

    // create and bind new OpenGL texture
    glGenTextures(1, &texture);
    glBindTexture(targetID, texture);

    // set texture environment parameters
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

    // set texture filtering
    glTexParameteri(targetID, GL_TEXTURE_MIN_FILTER, minFilter);
    glTexParameteri(targetID, GL_TEXTURE_MAG_FILTER, magFilter);

    // set texture wrapping mode
    glTexParameteri(targetID, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(targetID, GL_TEXTURE_WRAP_T, GL_REPEAT);

    // generate texture from bitmap data
    glTexImage2D(targetID, 0, format, width, height, 0, format, GL_UNSIGNED_BYTE, pPixels);

    return texture;
}
//--------------------------------------------------------------------------------------------------
#ifdef USE_SHADER
    GLint QR_OpenGLHelper::GetUniform(TQRShader* pShader, EQRShaderAttribute uniform)
    {
        // no shader?
        if (!pShader)
            return -1;

        // get uniform name as ansi string
        AnsiString nameA(pShader->GetAttributeName(uniform));

        // get uniform property name
        std::string propertyName = (nameA.c_str() ? nameA.c_str() : "");

        // found it?
        if (!propertyName.length())
            return -1;

        // get model matrix slot from shader
        return glGetUniformLocation(pShader->GetProgramID(), propertyName.c_str());
    }
#endif
//--------------------------------------------------------------------------------------------------
#ifdef USE_SHADER
    GLint QR_OpenGLHelper::GetAttribute(TQRShader* pShader, EQRShaderAttribute attribute)
    {
        // no shader?
        if (!pShader)
            return -1;

        // get attribute name as ansi string
        AnsiString nameA(pShader->GetAttributeName(attribute));

        // get attribute property name
        std::string propertyName = (nameA.c_str() ? nameA.c_str() : "");

        // found it?
        if (!propertyName.length())
            return -1;

        // get shader interpolation position attribute
        return glGetAttribLocation(pShader->GetProgramID(), propertyName.c_str());
    }
#endif
//--------------------------------------------------------------------------------------------------
void QR_OpenGLHelper::Draw(const TQRMesh&     mesh,
                           const TQRVector3D& translation,
                                 float        rotationX,
                                 float        rotationY,
                                 float        rotationZ,
                           const TQRVector3D& scale,
                           const TQRTextures& textures)
{
    // get mesh count
    const std::size_t count = mesh.Length;

    // no mesh to draw?
    if (!count)
        return;

    std::size_t stride;

    // calculate stride. As all meshes share the same vertex properties, the first mesh can be used
    // to extract vertex format info
    if (mesh[0].m_CoordType == EQR_VC_XYZ)
        stride = 3;
    else
        stride = 2;

    // do use normals array?
    if (mesh[0].m_Format.Contains(EQR_VF_Normals))
        stride += 3;

    // do use textures coordinates array?
    if (mesh[0].m_Format.Contains(EQR_VF_TexCoords))
        stride += 2;

    // do use colors array?
    if (mesh[0].m_Format.Contains(EQR_VF_Colors))
        stride += 4;

    glMatrixMode(GL_MODELVIEW);

    glPushMatrix();

    // place model into 3D world
    glTranslatef(translation.X, translation.Y, translation.Z);
    glRotatef(QR_MathsHelper::RadToDeg(rotationX), 1.0, 0.0, 0.0);
    glRotatef(QR_MathsHelper::RadToDeg(rotationY), 0.0, 1.0, 0.0);
    glRotatef(QR_MathsHelper::RadToDeg(rotationZ), 0.0, 0.0, 1.0);
    glScalef(scale.X, scale.Y, scale.Z);

    // iterate through vertices to draw
    for (std::size_t i = 0; i < count; ++i)
    {
        SelectTexture(textures, mesh[i].m_Name.c_str());

        // bind vertex array
        glEnableClientState(GL_VERTEX_ARRAY);
        glVertexPointer(3,
                        GL_FLOAT,
                        stride * sizeof(float),
                        &mesh[i].m_Buffer[0]);

        std::size_t offset = 3;

        // bind normals array
        if (mesh[i].m_Format.Contains(EQR_VF_Normals))
        {
            glEnableClientState(GL_NORMAL_ARRAY);
            glNormalPointer(GL_FLOAT,
                            stride * sizeof(float),
                            &mesh[i].m_Buffer[offset]);

            offset += 3;
        }

        // bind texture coordinates array
        if (mesh[i].m_Format.Contains(EQR_VF_TexCoords))
        {
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            glTexCoordPointer(2,
                              GL_FLOAT,
                              stride * sizeof(float),
                              &mesh[i].m_Buffer[offset]);

            offset += 2;
        }

        // bind colors array
        if (mesh[i].m_Format.Contains(EQR_VF_Colors))
        {
            glEnableClientState(GL_COLOR_ARRAY);
            glColorPointer(4,
                           GL_FLOAT,
                           stride * sizeof(float),
                           &mesh[i].m_Buffer[offset]);
        }

        // draw mesh
        switch (mesh[i].m_Type)
        {
            case EQR_VT_Triangles:     glDrawArrays(GL_TRIANGLES,      0, mesh[i].m_Buffer.Length / stride); break;
            case EQR_VT_TriangleStrip: glDrawArrays(GL_TRIANGLE_STRIP, 0, mesh[i].m_Buffer.Length / stride); break;
            case EQR_VT_TriangleFan:   glDrawArrays(GL_TRIANGLE_FAN,   0, mesh[i].m_Buffer.Length / stride); break;
            case EQR_VT_Quads:         glDrawArrays(GL_QUADS,          0, mesh[i].m_Buffer.Length / stride); break;
            case EQR_VT_QuadStrip:     glDrawArrays(GL_QUAD_STRIP,     0, mesh[i].m_Buffer.Length / stride); break;
            case EQR_VT_Unknown:
            default:                   throw "Unknown vertex type";
        }

        // unbind vertex array
        glDisableClientState(GL_VERTEX_ARRAY);

        // unbind normals array
        if (mesh[i].m_Format.Contains(EQR_VF_Normals))
            glDisableClientState(GL_NORMAL_ARRAY);

        // unbind texture coordinates array
        if (mesh[i].m_Format.Contains(EQR_VF_TexCoords))
            glDisableClientState(GL_TEXTURE_COORD_ARRAY);

        // unbind colors array
        if (mesh[i].m_Format.Contains(EQR_VF_Colors))
            glDisableClientState(GL_COLOR_ARRAY);

        glFlush();
    }

    glPopMatrix();
}
//--------------------------------------------------------------------------------------------------
void QR_OpenGLHelper::Draw(const TQRMesh&      mesh,
                           const TQRMatrix4x4& modelMatrix,
                           const TQRTextures&  textures)
{
    // get mesh count
    const std::size_t count = mesh.Length;

    // no mesh to draw?
    if (!count)
        return;

    std::size_t stride;

    // calculate stride. As all meshes share the same vertex properties, the first mesh can be used
    // to extract vertex format info
    if (mesh[0].m_CoordType == EQR_VC_XYZ)
        stride = 3;
    else
        stride = 2;

    // do use normals array?
    if (mesh[0].m_Format.Contains(EQR_VF_Normals))
        stride += 3;

    // do use textures coordinates array?
    if (mesh[0].m_Format.Contains(EQR_VF_TexCoords))
        stride += 2;

    // do use colors array?
    if (mesh[0].m_Format.Contains(EQR_VF_Colors))
        stride += 4;

    glMatrixMode(GL_MODELVIEW);

    glPushMatrix();

    // unfortunately, because Delphi don't allow to declare a const function as in C++
    TQRMatrix4x4* pModelMatrix = const_cast<TQRMatrix4x4*>(&modelMatrix);

    // place model into 3D world
    glLoadMatrixf(pModelMatrix->GetPtr());

    // iterate through vertices to draw
    for (std::size_t i = 0; i < count; ++i)
    {
        SelectTexture(textures, mesh[i].m_Name.c_str());

        // bind vertex array
        glEnableClientState(GL_VERTEX_ARRAY);
        glVertexPointer(3,
                        GL_FLOAT,
                        stride * sizeof(float),
                        &mesh[i].m_Buffer[0]);

        std::size_t offset = 3;

        // bind normals array
        if (mesh[i].m_Format.Contains(EQR_VF_Normals))
        {
            glEnableClientState(GL_NORMAL_ARRAY);
            glNormalPointer(GL_FLOAT,
                            stride * sizeof(float),
                            &mesh[i].m_Buffer[offset]);

            offset += 3;
        }

        // bind texture coordinates array
        if (mesh[i].m_Format.Contains(EQR_VF_TexCoords))
        {
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            glTexCoordPointer(2,
                              GL_FLOAT,
                              stride * sizeof(float),
                              &mesh[i].m_Buffer[offset]);

            offset += 2;
        }

        // bind colors array
        if (mesh[i].m_Format.Contains(EQR_VF_Colors))
        {
            glEnableClientState(GL_COLOR_ARRAY);
            glColorPointer(4,
                           GL_FLOAT,
                           stride * sizeof(float),
                           &mesh[i].m_Buffer[offset]);
        }

        // draw mesh
        switch (mesh[i].m_Type)
        {
            case EQR_VT_Triangles:     glDrawArrays(GL_TRIANGLES,      0, mesh[i].m_Buffer.Length / stride); break;
            case EQR_VT_TriangleStrip: glDrawArrays(GL_TRIANGLE_STRIP, 0, mesh[i].m_Buffer.Length / stride); break;
            case EQR_VT_TriangleFan:   glDrawArrays(GL_TRIANGLE_FAN,   0, mesh[i].m_Buffer.Length / stride); break;
            case EQR_VT_Quads:         glDrawArrays(GL_QUADS,          0, mesh[i].m_Buffer.Length / stride); break;
            case EQR_VT_QuadStrip:     glDrawArrays(GL_QUAD_STRIP,     0, mesh[i].m_Buffer.Length / stride); break;
            case EQR_VT_Unknown:
            default:                   throw "Unknown vertex type";
        }

        // unbind vertex array
        glDisableClientState(GL_VERTEX_ARRAY);

        // unbind normals array
        if (mesh[i].m_Format.Contains(EQR_VF_Normals))
            glDisableClientState(GL_NORMAL_ARRAY);

        // unbind texture coordinates array
        if (mesh[i].m_Format.Contains(EQR_VF_TexCoords))
            glDisableClientState(GL_TEXTURE_COORD_ARRAY);

        // unbind colors array
        if (mesh[i].m_Format.Contains(EQR_VF_Colors))
            glDisableClientState(GL_COLOR_ARRAY);

        glFlush();
    }

    glPopMatrix();
}
//--------------------------------------------------------------------------------------------------
#ifdef USE_SHADER
    bool QR_OpenGLHelper::Draw(const TQRMesh&      mesh,
                               const TQRMatrix4x4& modelMatrix,
                               const TQRTextures&  textures,
                                     TQRShader*    pShader)
    {
        // get mesh count
        const std::size_t count = mesh.Length;

        // no mesh to draw?
        if (!count)
            return false;

        // no shader program?
        if (!pShader)
            return false;

        try
        {
            // bind shader program
            pShader->Use(true);

            // get model matrix slot from shader
            GLint uniform = GetUniform(pShader, EQR_SA_ModelMatrix);

            // found it?
            if (uniform == -1)
                return false;

            // unfortunately, because Delphi don't allow to declare a const function as in C++
            TQRMatrix4x4* pModelMatrix = const_cast<TQRMatrix4x4*>(&modelMatrix);

            // connect model matrix to shader
            glUniformMatrix4fv(uniform, 1, Boolean(GL_FALSE), pModelMatrix->GetPtr());

            // get shader position attribute
            GLint posAttrib = GetAttribute(pShader, EQR_SA_Position);

            // found it?
            if (posAttrib == -1)
                return false;

            std::size_t stride;

            // calculate stride. As all meshes share the same vertex properties, the first mesh can
            // be used to extract vertex format info
            if (mesh[0].m_CoordType == EQR_VC_XYZ)
                stride = 3;
            else
                stride = 2;

            GLint normalAttrib = -1;

            // do use shader normal attribute?
            if (mesh[0].m_Format.Contains(EQR_VF_Normals))
            {
                // get shader normal attribute
                normalAttrib = GetAttribute(pShader, EQR_SA_Normal);

                // found it?
                if (normalAttrib == -1)
                    return false;

                stride += 3;
            }

            GLint uvAttrib = -1;

            // do use shader UV attribute?
            if (mesh[0].m_Format.Contains(EQR_VF_TexCoords))
            {
                // get shader UV attribute
                uvAttrib = GetAttribute(pShader, EQR_SA_Texture);

                // found it?
                if (uvAttrib == -1)
                    return false;

                // add texture coordinates to stride
                stride += 2;
            }

            GLint colorAttrib = -1;

            // do use shader color attribute?
            if (mesh[0].m_Format.Contains(EQR_VF_Colors))
            {
                // get shader color attribute
                colorAttrib = GetAttribute(pShader, EQR_SA_Color);

                // found it?
                if (colorAttrib == -1)
                    return false;

                // add color to stride
                stride += 4;
            }

            // iterate through OpenGL meshes
            for (std::size_t i = 0; i < count; ++i)
            {
                SelectTexture(pShader, textures, mesh[i].m_Name);

                std::size_t offset = 0;

                // connect vertices to vertex shader position attribute
                glEnableVertexAttribArray(posAttrib);
                glVertexAttribPointer(posAttrib,
                                      3,
                                      GL_FLOAT,
                                      GL_FALSE,
                                      stride * sizeof(float),
                                      &mesh[i].m_Buffer[offset]);

                if (mesh[i].m_CoordType == EQR_VC_XYZ)
                    offset = 3;
                else
                    offset = 2;

                // vertex buffer contains normals?
                if (normalAttrib != -1)
                {
                    // connect the vertices to the vertex shader normal attribute
                    glEnableVertexAttribArray(normalAttrib);
                    glVertexAttribPointer(normalAttrib,
                                          3,
                                          GL_FLOAT,
                                          GL_FALSE,
                                          stride * sizeof(float),
                                          &mesh[i].m_Buffer[offset]);

                    offset += 3;
                }

                // vertex buffer contains texture coordinates?
                if (uvAttrib != -1)
                {
                    // connect the color to the vertex shader vColor attribute and redirect to
                    // the fragment shader
                    glEnableVertexAttribArray(uvAttrib);
                    glVertexAttribPointer(uvAttrib,
                                          2,
                                          GL_FLOAT,
                                          GL_FALSE,
                                          stride * sizeof(float),
                                          &mesh[i].m_Buffer[offset]);

                    offset += 2;
                }

                // vertex buffer contains colors?
                if (colorAttrib != -1)
                {
                    // connect the color to the vertex shader vColor attribute and redirect to
                    // the fragment shader
                    glEnableVertexAttribArray(colorAttrib);
                    glVertexAttribPointer(colorAttrib,
                                          4,
                                          GL_FLOAT,
                                          GL_FALSE,
                                          stride * sizeof(float),
                                          &mesh[i].m_Buffer[offset]);
                }

                // draw mesh
                switch (mesh[i].m_Type)
                {
                    case EQR_VT_Triangles:     glDrawArrays(GL_TRIANGLES,      0, mesh[i].m_Buffer.Length / stride); break;
                    case EQR_VT_TriangleStrip: glDrawArrays(GL_TRIANGLE_STRIP, 0, mesh[i].m_Buffer.Length / stride); break;
                    case EQR_VT_TriangleFan:   glDrawArrays(GL_TRIANGLE_FAN,   0, mesh[i].m_Buffer.Length / stride); break;
                    case EQR_VT_Quads:         glDrawArrays(GL_QUADS,          0, mesh[i].m_Buffer.Length / stride); break;
                    case EQR_VT_QuadStrip:     glDrawArrays(GL_QUAD_STRIP,     0, mesh[i].m_Buffer.Length / stride); break;
                    case EQR_VT_Unknown:
                    default:                   throw "Unknown vertex type";
                }
            }
        }
        __finally
        {
            // unbind shader program
            pShader->Use(false);
        }

        return true;
    }
#endif
//--------------------------------------------------------------------------------------------------
#ifdef USE_SHADER
    bool QR_OpenGLHelper::Draw(const TQRMesh&      mesh,
                               const TQRMesh&      nextMesh,
                               const TQRMatrix4x4& modelMatrix,
                                     float         interpolationFactor,
                               const TQRTextures&  textures,
                                     TQRShader*    pShader)
    {
        // get mesh count
        const std::size_t count = mesh.Length;

        // no mesh to draw?
        if (!count)
            return false;

        // no shader program?
        if (!pShader)
            return false;

        try
        {
            // bind shader program
            pShader->Use(true);

            // get model matrix slot from shader
            GLint uniform = GetUniform(pShader, EQR_SA_ModelMatrix);

            // found it?
            if (uniform == -1)
                return false;

            // unfortunately, because Delphi don't allow to declare a const function as in C++
            TQRMatrix4x4* pModelMatrix = const_cast<TQRMatrix4x4*>(&modelMatrix);

            // connect model matrix to shader
            glUniformMatrix4fv(uniform, 1, Boolean(GL_FALSE), pModelMatrix->GetPtr());

            // get shader position attribute
            GLint interpolationAttrib = GetUniform(pShader, EQR_SA_Interpolation);

            // found interpolation attribute?
            if (interpolationAttrib == -1)
                return false;

            // send interpolation factor to shader program
            glUniform1f(interpolationAttrib, interpolationFactor);

            // get shader position attribute
            GLint posAttrib = GetAttribute(pShader, EQR_SA_Position);

            // found it?
            if (posAttrib == -1)
                return false;

            // get shader interpolation position attribute
            GLint iPosAttrib = GetAttribute(pShader, EQR_SA_InterpolationPos);

            // found it?
            if (iPosAttrib == -1)
                return false;

            std::size_t stride;

            // calculate stride. As all meshes share the same vertex properties, the first mesh can
            // be used to extract vertex format info
            if (mesh[0].m_CoordType == EQR_VC_XYZ)
                stride = 3;
            else
                stride = 2;

            GLint normalAttrib  = -1;
            GLint iNormalAttrib = -1;

            // do use shader normal attribute?
            if (mesh[0].m_Format.Contains(EQR_VF_Normals))
            {
                // get shader normal attribute
                normalAttrib = GetAttribute(pShader, EQR_SA_Normal);

                // found it?
                if (normalAttrib == -1)
                    return false;

                // get shader normal attribute
                iNormalAttrib = GetAttribute(pShader, EQR_SA_InterpolationNormal);

                // found it?
                if (iNormalAttrib == -1)
                    return false;

                stride += 3;
            }

            GLint uvAttrib = -1;

            // do use shader UV attribute?
            if (mesh[0].m_Format.Contains(EQR_VF_TexCoords))
            {
                // get shader UV attribute
                uvAttrib = GetAttribute(pShader, EQR_SA_Texture);

                // found it?
                if (uvAttrib == -1)
                    return false;

                // add texture coordinates to stride
                stride += 2;
            }

            GLint colorAttrib = -1;

            // do use shader color attribute?
            if (mesh[0].m_Format.Contains(EQR_VF_Colors))
            {
                // get shader color attribute
                colorAttrib = GetAttribute(pShader, EQR_SA_Color);

                // found it?
                if (colorAttrib == -1)
                    return false;

                // add color to stride
                stride += 4;
            }

            // iterate through OpenGL meshes
            for (std::size_t i = 0; i < count; ++i)
            {
                SelectTexture(pShader, textures, mesh[i].m_Name);

                std::size_t offset = 0;

                // connect vertices to vertex shader position attribute
                glEnableVertexAttribArray(posAttrib);
                glVertexAttribPointer(posAttrib,
                                      3,
                                      GL_FLOAT,
                                      GL_FALSE,
                                      stride * sizeof(float),
                                      &mesh[i].m_Buffer[offset]);

                // connect vertices to vertex shader position attribute
                glEnableVertexAttribArray(iPosAttrib);
                glVertexAttribPointer(iPosAttrib,
                                      3,
                                      GL_FLOAT,
                                      GL_FALSE,
                                      stride * sizeof(float),
                                      &nextMesh[i].m_Buffer[offset]);

                if (mesh[i].m_CoordType == EQR_VC_XYZ)
                    offset = 3;
                else
                    offset = 2;

                // vertex buffer contains normals?
                if (normalAttrib != -1)
                {
                    // connect the normals to the vertex shader normal attribute
                    glEnableVertexAttribArray(normalAttrib);
                    glVertexAttribPointer(normalAttrib,
                                          3,
                                          GL_FLOAT,
                                          GL_FALSE,
                                          stride * sizeof(float),
                                          &mesh[i].m_Buffer[offset]);

                    // vertex buffer contains interpolated normals?
                    if (iNormalAttrib != -1)
                    {
                        // connect the interpolated normals to the vertex shader normal attribute
                        glEnableVertexAttribArray(iNormalAttrib);
                        glVertexAttribPointer(iNormalAttrib,
                                              3,
                                              GL_FLOAT,
                                              GL_FALSE,
                                              stride * sizeof(float),
                                              &nextMesh[i].m_Buffer[offset]);
                    }

                    offset += 3;
                }

                // vertex buffer contains texture coordinates?
                if (uvAttrib != -1)
                {
                    // connect the color to the vertex shader vColor attribute and redirect to
                    // the fragment shader
                    glEnableVertexAttribArray(uvAttrib);
                    glVertexAttribPointer(uvAttrib,
                                          2,
                                          GL_FLOAT,
                                          GL_FALSE,
                                          stride * sizeof(float),
                                          &mesh[i].m_Buffer[offset]);

                    offset += 2;
                }

                // vertex buffer contains colors?
                if (colorAttrib != -1)
                {
                    // connect the color to the vertex shader vColor attribute and redirect to
                    // the fragment shader
                    glEnableVertexAttribArray(colorAttrib);
                    glVertexAttribPointer(colorAttrib,
                                          4,
                                          GL_FLOAT,
                                          Boolean(GL_FALSE),
                                          stride * sizeof(float),
                                          &mesh[i].m_Buffer[offset]);
                }

                // draw mesh
                switch (mesh[i].m_Type)
                {
                    case EQR_VT_Triangles:     glDrawArrays(GL_TRIANGLES,      0, mesh[i].m_Buffer.Length / stride); break;
                    case EQR_VT_TriangleStrip: glDrawArrays(GL_TRIANGLE_STRIP, 0, mesh[i].m_Buffer.Length / stride); break;
                    case EQR_VT_TriangleFan:   glDrawArrays(GL_TRIANGLE_FAN,   0, mesh[i].m_Buffer.Length / stride); break;
                    case EQR_VT_Quads:         glDrawArrays(GL_QUADS,          0, mesh[i].m_Buffer.Length / stride); break;
                    case EQR_VT_QuadStrip:     glDrawArrays(GL_QUAD_STRIP,     0, mesh[i].m_Buffer.Length / stride); break;
                    case EQR_VT_Unknown:
                    default:                   throw "Unknown vertex type";
                }
            }
        }
        __finally
        {
            // unbind shader program
            pShader->Use(false);
        }

        return true;
    }
#endif
//--------------------------------------------------------------------------------------------------
void QR_OpenGLHelper::SelectTexture(const TQRTextures& textures, const UnicodeString& modelName)
{
    // do draw textures?
    if (!textures.Length)
    {
        glDisable(GL_TEXTURE_2D);
        return;
    }

    int index = -1;

    // iterate through textures belonging to model
    for (int i = 0; i < textures.Length; ++i)
        // found a texture to draw?
        if (textures[i] && textures[i]->Enabled && textures[i]->Name == modelName)
        {
            // get texture index
            index = i;
            break;
        }

    // found texture index to draw?
    if (index >= 0)
    {
        // draw texture
        glEnable(GL_TEXTURE_2D);
        glBindTexture(GL_TEXTURE_2D, textures[index]->Index);
        return;
    }

    glDisable(GL_TEXTURE_2D);
}
//--------------------------------------------------------------------------------------------------
#ifdef USE_SHADER
    void QR_OpenGLHelper::SelectTexture(      TQRShader*     pShader,
                                        const TQRTextures&   textures,
                                        const UnicodeString& modelName)
    {
        // get color map slot from shader
        GLint uniform = GetUniform(pShader, EQR_SA_ColorMap);

        // found it?
        if (uniform == -1)
            // nothing to do (some shader may have no texture to handle)
            return;

        // do draw textures?
        if (!textures.Length)
        {
            glDisable(GL_TEXTURE_2D);
            return;
        }

        int index = -1;

        // iterate through textures belonging to model
        for (int i = 0; i < textures.Length; ++i)
            // found a texture to draw?
            if (textures[i] && textures[i]->Enabled && textures[i]->Name == modelName)
            {
                // get texture index
                index = i;
                break;
            }

        // found texture index to draw?
        if (index >= 0)
        {
            // draw texture
            glEnable(GL_TEXTURE_2D);
            glBindTexture(GL_TEXTURE_2D, textures[index]->Index);
            glActiveTexture(GL_TEXTURE0);
            return;
        }

        glDisable(GL_TEXTURE_2D);
    }
#endif
//--------------------------------------------------------------------------------------------------
