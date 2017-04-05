/*************************************************************************************************
 * ==> Main -------------------------------------------------------------------------------------*
 *************************************************************************************************
 * Description : 3D geometrical shapes demo main form                                            *
 * Developer   : Jean-Milost Reymond                                                             *
 *************************************************************************************************
 * MIT License - The Mels Library, a free and easy-to-use 3D Models library                      *
 *                                                                                               *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software *
 * and associated documentation files (the "Software"), to deal in the Software without          *
 * restriction, including without limitation the rights to use, copy, modify, merge, publish,    *
 * distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the *
 * Software is furnished to do so, subject to the following conditions:                          *
 *                                                                                               *
 * The above copyright notice and this permission notice shall be included in all copies or      *
 * substantial portions of the Software.                                                         *
 *                                                                                               *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING *
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND    *
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,  *
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *
 *************************************************************************************************/

#ifndef MainH
#define MainH

// vcl
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Menus.hpp>

// std
#include <time.h>

// Mels library
#include <UTQRShapeGroup.hpp>

// opengl
#include <gl\gl.h>

/**
* 3D geometrical shapes demo main form
*@author Jean-Milost Reymond
*/
class TMainForm : public TForm
{
    __published:
        TPopupMenu *pmOptions;
        TMenuItem *miLighting;

        void __fastcall FormCreate(TObject* pSender);
        void __fastcall FormResize(TObject* pSender);
        void __fastcall FormPaint(TObject* pSender);
        void __fastcall miLightingClick(TObject* pSender);

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
        * Called when thread do nothing else
        *@param pSender - event sender
        *@param[in, out] done - if true, idle loop was ternimated and may do nothing else
        */
        void __fastcall OnIdle(TObject* pSender, bool& done);

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
        HDC               m_hDC;
        HGLRC             m_hRC;
        TQRSurfaceGroup*  m_pSurface;
        TQRSphereGroup*   m_pSphere;
        TQRBoxGroup*      m_pBox;
        TQRConeGroup*     m_pCone;
        TQRTorusGroup*    m_pTorus;
        TQRParabolaGroup* m_pParabola;
        TQRConeGroup*     m_pCylinder;
        TQRConeGroup*     m_pPyramid;
        TQRSphereGroup*   m_pTetrahedron;
        std::time_t       m_PreviousTime;

        /**
        * Configures OpenGL
        */
        void ConfigOpenGL();

        /**
        * Loads texture from resource
        *@param resIndex - resource index
        *@return OpenGL texture index, 0 on error or if failed
        */
        GLint LoadTexture(int resIndex) const;

        /**
        * Called when mesh texture should be loaded
        *@param pModel - model for which texture should be loaded
        *@param pBitmap - whenever possible, the bitmap containing the texture, nil if not available
        *@param pTexture - texture info
        *@param [out] loadNext - if true, event will be called again with a new item to load next texture
        *@return true on success, otherwise false
        */
        bool __fastcall OnLoadMeshTexture(TQRModelGroup* const pGroup,
                                          TQRModel* const      pModel,
                                          TBitmap*             pBitmap,
                                          TQRTexture*          pTexture,
                                          bool&                loadNext);

        /**
        * Called when static model will be drawn and caller should extract mesh
        *@param pGroup - group at which model belongs
        *@param pModel - model to draw
        *@param textures - texture list, in order they should be linked
        *@param matrix - model matrix
        */
        void __fastcall OnDrawCustomStaticModelItem(TQRModelGroup*      pGroup,
                                                    TQRModel*           pModel,
                                                    const TQRTextures   textures,
                                                    const TQRMatrix4x4& matrix);
};
extern PACKAGE TMainForm* MainForm;
#endif
