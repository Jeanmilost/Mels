/******************************************************************************
 * ==> Main ------------------------------------------------------------------*
 ******************************************************************************
 * Description : MD3 demo main form                                           *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef MainH
#define MainH

// vcl
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.Forms.hpp>

// qr engine
#include "QR_MD3.h"
#include "QR_MD3Animation.h"
#include "QR_MD3Group.h"

// opengl
#include <gl\gl.h>
#include <gl\glu.h>

/**
* MD3 demo main form
*@author Jean-Milost Reymond
*/
class TMainForm : public TForm
{
    __published:
        TPopupMenu *pmOptions;
        TMenuItem *miSeparator1;
        TMenuItem *miPrevTorsoAnim;
        TMenuItem *miNextTorsoAnim;
        TMenuItem *miPrevLegsAnim;
        TMenuItem *miNextLegsAnim;

        void __fastcall FormCreate(TObject* pSender);
        void __fastcall FormResize(TObject* pSender);
        void __fastcall FormPaint(TObject* pSender);
        void __fastcall miPrevTorsoAnimClick(TObject* pSender);
        void __fastcall miNextTorsoAnimClick(TObject* pSender);
        void __fastcall miPrevLegsAnimClick(TObject* pSender);
        void __fastcall miNextLegsAnimClick(TObject* pSender);

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
        HDC                        m_hDC;
        HGLRC                      m_hRC;
        QR_MD3Group                m_MD3Model;
        QR_ModelGroup::IMemoryDir* m_pMemoryDir;
        std::time_t                m_PreviousTime;
        int                        m_CurTorsoGesture;
        int                        m_CurLegsGesture;

        /**
        * Loads all resources and add them in memory directory
        *@return true on success, otherwise false
        */
        bool CreateAndPopulateMemoryDir();

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
        *@return newly created texture identifier, 0 on error
        */
        GLuint CreateTexture(WORD width, WORD height, WORD format, void* pPixels, GLuint minFilter,
                GLuint magFilter, GLuint targetID);

        /**
        * Configures OpenGL
        */
        void ConfigOpenGL();

        /**
        * Called when texture should be loaded
        *@param[in, out] pTexture - texture info, contains loaded index when function ends
        *@return true on success, otherwise false
        */
        bool OnLoadTexture(QR_ModelGroup::ITexture* pTexture);

        /**
        * Called when model item should be drawn
        *@param pModel - model to draw
        *@param textures - textures belonging to model, in the order where they should be combined
        *@param matrix - model matrix
        *@param pMesh - mesh to draw
        *@param pNextMesh - next mesh to interpolate with, can be NULL
        *@param interpolationFactor - interpolation factor
        */
        void OnDrawItem(const QR_Model*                 pModel,
                        const QR_ModelGroup::ITextures& textures,
                        const QR_Matrix16P&             matrix,
                        const QR_Mesh*                  pMesh,
                        const QR_Mesh*                  pNextMesh,
                              QR_Double                 interpolationFactor);
};
extern PACKAGE TMainForm* MainForm;
#endif
