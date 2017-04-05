/*************************************************************************************************
 * ==> Options form -----------------------------------------------------------------------------*
 *************************************************************************************************
 * Description : Form to allow user to select which and how the MD2 model will be rendered       *
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

#ifndef TOptionsH
#define TOptionsH

// vcl
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

// Mels library
#include <UTQRMD2ModelGroup.hpp>

/**
* User options form
*@author Jean-Milost Reymond
*/
class TOptions : public TForm
{
    __published:
        TRadioGroup *rgCacheOptions;
        TGroupBox *gbLoadOptions;
        TCheckBox *ckShowDefaultFrame;
        TCheckBox *ckRunGestureWhenReady;
        TGroupBox *gbRenderOptions;
        TCheckBox *ckShowCollisions;
        TCheckBox *ckUseShader;
        TImage *imPreview;
        TPanel *paButtons;
        TButton *btOK;
        TButton *btCancel;
        TButton *btQuit;
        TPanel *paPreview;
        TCheckBox *ckFullScreen;
        TPanel *paMain;
        TTimer *tiDrawPreview;
        TCheckBox *ckPreCalculateLight;
        TCheckBox *ckUseOrthoMatrix;

        void __fastcall FormCreate(TObject* pSender);
        void __fastcall rgCacheOptionsClick(TObject* pSender);
        void __fastcall btQuitClick(TObject* pSender);
        void __fastcall btCancelClick(TObject* pSender);
        void __fastcall btOKClick(TObject* pSender);
        void __fastcall tiDrawPreviewTimer(TObject* pSender);

    public:
        /**
        * Constructor
        *@param pOwner - form owner
        */
        __fastcall TOptions(TComponent* pOwner);

        /**
        * Destructor
        */
        __fastcall ~TOptions();

        /**
        * Checks if application is closing
        *@return true if application is closing, otherwise false
        */
        bool IsAppClosing() const;

    protected:
        /**
        * Options form message loop
        *@param message - message sent by Windows
        */
        void __fastcall WndProc(TMessage& message);

    private:
        TQRMD2Group* m_pMD2;
        bool         m_ModelRendered;
        bool         m_Closing;

        /**
        * Resets interface to default values
        */
        void Reset();

        /**
        * Loads preview image
        */
        void LoadPreview();

        /**
        * Called when mesh texture should be loaded
        *@param pGroup - group at which model belongs
        *@param pModel - model for which texture should be loaded
        *@param pBitmap - whenever possible, the bitmap containing the texture, nil if not available
        *@param pTexture - texture info
        *@param[out] loadNext - if true, event will be called again with a new item to load next texture
        *@return true on success, otherwise false
        */
        bool __fastcall OnLoadMeshTexture(TQRModelGroup* const pGroup,
                                          TQRModel* const      pModel,
                                          TBitmap*             pBitmap,
                                          TQRTexture*          pTexture,
                                          bool&                loadNext);

        /**
        * Called when framed model item should be drawn
        *@param pGroup - group at which model belongs
        *@param pModel - model to draw
        *@param textures - textures belonging to model, in the order where they should be combined
        *@param matrix - model matrix
        *@param index - model mesh index
        *@param nextIndex - model mesh index to interpolate with
        *@param interpolationFactor - interpolation factor
        */
        void __fastcall OnDrawCustomModelItem(TQRModelGroup* const pGroup,
                                              TQRModel*            pModel,
                                              const TQRTextures    textures,
                                              const TQRMatrix4x4&  matrix,
                                              NativeInt            index,
                                              NativeInt            nextIndex,
                                              const double         interpolationFactor);
};
extern PACKAGE TOptions* Options;
#endif
