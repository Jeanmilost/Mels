/**************************************************************************************************
 * ==> Options form ------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Form to allow user to select which and how the MD3 model will be rendered        *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#ifndef TOptionsH
#define TOptionsH

// vcl
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Dialogs.hpp>

// std
#include <set>

// Mels library
#include <UTQRMD3ModelGroup.hpp>

/**
* User options form
*@author Jean-Milost Reymond
*/
class TOptions : public TForm
{
    __published:
        TRadioGroup *rgCacheOptions;
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
        TGroupBox *gbLoadModel;
        TEdit *edModelFileName;
        TButton *btBrowse;
        TOpenDialog *odOpenDialog;
        TGroupBox *gbSelectTeam;
        TRadioButton *rbDefault;
        TRadioButton *rbRed;
        TRadioButton *rbBlue;

        void __fastcall FormCreate(TObject* pSender);
        void __fastcall rgCacheOptionsClick(TObject* pSender);
        void __fastcall btQuitClick(TObject* pSender);
        void __fastcall btCancelClick(TObject* pSender);
        void __fastcall btOKClick(TObject* pSender);
        void __fastcall tiDrawPreviewTimer(TObject* pSender);
        void __fastcall btBrowseClick(TObject* pSender);
        void __fastcall OnSelectTeam(TObject* pSender);

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
        * Gets selected team
        *@return selected team
        */
        EQRMD3PackageTeam GetSelectedTeam() const;

    protected:
        /**
        * Options form message loop
        *@param message- message sent by Windows
        */
        void __fastcall WndProc(TMessage& message);

    private:
        typedef std::set<TBitmap*> ITextures;

        TQRMD3Group*      m_pMD3;
        ITextures         m_Textures;
        EQRMD3PackageTeam m_Team;
        bool              m_ModelRendered;

        /**
        * Resets interface to default values
        */
        void Reset();

        /**
        * Loads preview image
        *@param pStream - model stream for which preview should be shown
        *@return true on success, otherwise false
        */
        bool LoadPreview(TStream* pStream);

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
