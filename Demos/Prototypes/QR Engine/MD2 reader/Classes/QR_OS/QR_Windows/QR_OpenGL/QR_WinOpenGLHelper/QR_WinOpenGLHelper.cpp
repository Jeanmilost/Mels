/******************************************************************************
 * ==> QR_WinOpenGLHelper ----------------------------------------------------*
 ******************************************************************************
 * Description : OpenGL helper in the Windows API context                     *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_WinOpenGLHelper.h"

//------------------------------------------------------------------------------
// QR_WinOpenGLHelper - c++
//------------------------------------------------------------------------------
QR_WinOpenGLHelper::QR_WinOpenGLHelper()
{}
//------------------------------------------------------------------------------
QR_WinOpenGLHelper::~QR_WinOpenGLHelper()
{}
//------------------------------------------------------------------------------
bool QR_WinOpenGLHelper::EnableOpenGL(HWND hWnd, HDC& hDC, HGLRC& hRC)
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
//------------------------------------------------------------------------------
void QR_WinOpenGLHelper::DisableOpenGL(HWND hWnd, HDC hDC, HGLRC hRC)
{
    // disable OpenGL context
    wglMakeCurrent(NULL, NULL);

    // delete OpenGL context
    if (hRC)
        wglDeleteContext(hRC);

    // delete device context
    if (hWnd && hDC)
        ::ReleaseDC(hWnd, hDC);
}
//------------------------------------------------------------------------------
bool QR_WinOpenGLHelper::SetPixelFormat(HDC hDC)
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
        0, 0, 0, 0, 0, 0,
        0, 0,
        0, 0, 0, 0, 0,
        32,
        0,
        0,
        PFD_MAIN_PLANE,
        0,
        0, 0,
    };

    // get best available pixel format
    const int pixelFormat = ::ChoosePixelFormat(hDC, &pfd);

    // set pixel format to use
    return ::SetPixelFormat(hDC, pixelFormat, &pfd);
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_WinOpenGLHelper::MousePosToGLPoint(HWND hWnd, const QR_Rect<M_Precision>& viewRect)
{
    // no window or control?
    if (!hWnd)
        return QR_Vector3DP();

    ::POINT p;

    // get mouse position
    ::GetCursorPos(&p);

    // convert to window or control client coordinates
    if (!::ScreenToClient(hWnd, &p))
        return QR_Vector3DP();

    const M_Precision mouseX = p.x;
    const M_Precision mouseY = p.y;

    ::RECT clientRect;

    // get window or control client rect
    ::GetClientRect(hWnd, &clientRect);

    // calculate client width and height
    const M_Precision clientWidth  = (clientRect.right  - clientRect.left);
    const M_Precision clientHeight = (clientRect.bottom - clientRect.top);

    // convert mouse position to OpenGL position
    return QR_Vector3DP(viewRect.m_Left + ((mouseX * viewRect.m_Width)  / clientWidth),
                        viewRect.m_Top  - ((mouseY * viewRect.m_Height) / clientHeight),
                        0.0f);
}
//------------------------------------------------------------------------------
