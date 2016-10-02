/******************************************************************************
 * ==> Main ------------------------------------------------------------------*
 ******************************************************************************
 * Description : MD2 demo main form                                           *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef MainH
#define MainH

// vcl
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>

// qr engine
#include "QR_MD2Collision.h"
#include "QR_MD2.h"
#include "QR_MD2Animation.h"
#include <Vcl.Menus.hpp>

// opengl
#include <gl\gl.h>
#include <gl\glu.h>

/**
* MD2 demo main form
*@author Jean-Milost Reymond
*/
class TMainForm : public TForm
{
    __published:
        TPopupMenu *pmOptions;
        TMenuItem *miLighting;
        TMenuItem *miSeparator1;
        TMenuItem *miPrevAnim;
        TMenuItem *miNextAnim;

        void __fastcall FormCreate(TObject* pSender);
        void __fastcall FormResize(TObject* pSender);
        void __fastcall FormPaint(TObject* pSender);
        void __fastcall miLightingClick(TObject* pSender);
        void __fastcall miPrevAnimClick(TObject* pSender);
        void __fastcall miNextAnimClick(TObject* pSender);

    public:
        /**
        * Constructor
        *@param pOwner - form owner
        */
        __fastcall TMainForm(TComponent* pOwner);

        /**
        * Destructor
        */
        virtual __fastcall ~TMainForm();

    protected:
        /**
        * Configure OpenGL with correct format descriptor
        */
        void __fastcall SetPixelFormatDescriptor();

        /**
        * Called when thread do nothing else
        *@param pSEnder - event sender
        *@param[in, out] done - if true, idle loop was ternimated and may do nothing else
        */
        void __fastcall IdleLoop(TObject* pSender, bool& done);

        /**
        * Renders (i.e. prepares and draws) scene
        */
        void __fastcall RenderGLScene();

        /**
        * Draws scene
        *@param elapsedTime - elapsed time since last draw
        */
        void __fastcall Draw(const double& elapsedTime);

    private:
        HDC              m_hDC;
        HGLRC            m_hRC;
        QR_MD2           m_MD2;
        QR_MD2Animation* m_pMD2Animation;
        QR_MD2Collision  m_MD2Collision;
        TBitmap*         m_pTexture;
        std::time_t      m_PreviousTime;

        /**
        * Convert bitmap in pixels byte array
        *@param pBitmap - bitmap to convert
        *@param[out] pPixels - pixels array when function returns, user is responsible to delete it
        *@param flipY - if true, image will be mirrored on the Y axis
        *@param bgr - if true, image will be converted from RGB to BGR (or RGBA to ABGR) format
        *@return true on success, otherwise false
        */
        bool BytesFromBitmap(TBitmap* pBitmap, BYTE*& pPixels, bool flipY, bool bgr);

        /**
        * Creates texture
        *@param width - texture width
        *@param height - texture height
        *@param format - texture format, can be GL_RGB or GL_RGBA
        *@param pPixels - texture pixels array
        *@param minFiltter - min filter to apply
        *@param magFilter - mag filter to apply
        *@param targetID - OpenGL target identigier, e.g. GL_TEXTURE_2D
        *@return newly created texture identifier
        */
        int CreateTexture(WORD width, WORD height, WORD format, void* pPixels, GLuint minFilter,
                GLuint magFilter, GLuint targetID);

        /**
        * Builds animation list
        */
        void BuildAnimation();

        /**
        * Configures OpenGL
        */
        void ConfigOpenGL();
};
extern PACKAGE TMainForm* MainForm;
#endif
