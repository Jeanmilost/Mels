/**************************************************************************************************
 * ==> Main --------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : MD3 demo main form                                                               *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#ifndef MainH
#define MainH

// vcl
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

// std
#include <map>
#include <time.h>

// Mels library
#include <UTQRMD3ModelGroup.hpp>

// engine
#include "QR_Shader_OpenGL.h"

// interface
#include "TOptions.h"

/**
* MD3 demo main form
*@author Jean-Milost Reymond
*/
class TMainForm : public TForm
{
    __published:
        TPopupMenu *pmOptions;
        TMenuItem *miPrevTorsoAnim;
        TMenuItem *miNextTorsoAnim;
        TProgressBar *pbLoadModel;
        TPanel *paRendering;
        TMenuItem *miSeparator;
        TMenuItem *miPrevLegsAnim;
        TMenuItem *miNextLegsAnim;

        void __fastcall FormCreate(TObject* pSender);
        void __fastcall FormResize(TObject* pSender);
        void __fastcall FormKeyPress(TObject* pSender, WideChar& key);
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
        * Called when thread do nothing else
        *@param pSEnder - event sender
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
        /**
        * Frame structure, contains the local model cache
        */
        struct IFrame
        {
            TQRMesh*     m_pMesh;
            TQRAABBTree* m_pAABBTree;

            IFrame(bool useCollisions);
            virtual ~IFrame();
        };

        typedef std::map<std::size_t, IFrame*>  IFrames;
        typedef std::map<TQRModel*,   IFrames*> ICache;

        ICache            m_Cache;
        TOptions*         m_pOptions;
        HDC               m_hDC;
        HGLRC             m_hRC;
        TQRMD3Group*      m_pMD3;
        QR_Shader_OpenGL* m_pTextureShader;
        QR_Shader_OpenGL* m_pColorShader;
        TQRMatrix4x4      m_ProjectionMatrix;
        TQRMatrix4x4      m_ViewMatrix;
        std::time_t       m_PreviousTime;
        int               m_CurTorsoGesture;
        int               m_CurLegsGesture;
        bool              m_AnimCached;
        bool              m_Cached;

        /**
        * Configures OpenGL
        */
        void ConfigOpenGL();

        /**
        * Builds shader
        *@param pVertexPrg - stream containing vertex program
        *@param pFragmentPrg - stream containing fragment program
        *@param pShader - shader to populate
        *@return true on success, otherwise false
        */
        bool BuildShader(TStream* pVertexPrg, TStream* pFragmentPrg, QR_Shader_OpenGL* pShader);

        /**
        * Loads MD3 model
        *@return true on success, otherwise false
        */
        bool LoadModel();

        /**
        * Updates cache progress bar
        */
        void UpdateCacheProgress();

        /**
        * Gets frame from local cache
        *@param index - frame index
        *@param pModel - model for which next frame should be get
        *@param useCollision - if true, collisions are used
        *@return frame
        */
        IFrame* GetFrame(std::size_t index, TQRMD3Model* pModel, bool useCollision);

        /**
        * Detects collision with mouse pointer and draws the polygons in collision
        *@param modelMatrix - model matrix
        *@param pAABBTree - aligned-axis bounding box tree
        */
        void DetectAndDrawCollisions(const TQRMatrix4x4& modelMatrix, TQRAABBTree* pAABBTree);

        /**
        * Prepres the shader to draw the model
        *@param pShader - shader to prepare
        *@param modelName - model name
        *@param textures - textures belonging to model
        */
        void PrepareShaderToDrawModel(      QR_Shader_OpenGL* pShader,
                                      const UnicodeString&    modelName,
                                      const TQRTextures&      textures);

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
        * Called when framed model item should be drawn
        *@param pGroup - group at which model belongs
        *@param pModel - model to draw
        *@param textures - textures belonging to model, in the order where they should be combined
        *@param matrix - model matrix
        *@param index - model mesh index
        *@param nextIndex - model mesh index to interpolate with
        *@param interpolationFactor - interpolation factor
        *@param pMesh - mesh to draw, can be NULL
        *@param pNextMesh - next mesh to interpolate with, can be NULL
        *@param pAABBTree - aligned-axis bounding box tree matching with mesh, can be NULL
        *@param pNextAABBTree - aligned-axis bounding box tree matching with next mesh, can be NULL
        */
        void __fastcall OnDrawModelItem(TQRModelGroup* const pGroup,
                                        TQRModel* const      pModel,
                                        const TQRTextures    textures,
                                        const TQRMatrix4x4&  matrix,
                                        NativeInt            index,
                                        NativeInt            nextIndex,
                                        const double         interpolationFactor,
                                        const PQRMesh        pMesh,
                                        const PQRMesh        pNextMesh,
                                        TQRAABBTree* const   pTree,
                                        TQRAABBTree* const   pNextTree);

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
extern PACKAGE TMainForm* MainForm;
#endif
