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
#include <UTQRMD3.hpp>

// engine
#include "QR_Shader_OpenGL.h"

/**
* MD3 demo main form
*@author Jean-Milost Reymond
*/
class TMainForm : public TForm
{
    __published:
        TPanel *paRendering;

        void __fastcall FormCreate(TObject* pSender);
        void __fastcall FormResize(TObject* pSender);
        void __fastcall FormKeyPress(TObject* pSender, WideChar& key);
        void __fastcall FormPaint(TObject* pSender);

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

        typedef std::map<std::size_t, IFrame*> IFrames;

        static bool        m_FullScreen;
        static bool        m_UseShader;
        static bool        m_Collisions;
        static std::size_t m_FPS;
        IFrames            m_Frames;
        HDC                m_hDC;
        HGLRC              m_hRC;
        TQRMD3Model*       m_pMD3;
        QR_Shader_OpenGL*  m_pColorShader;
        QR_Shader_OpenGL*  m_pTextureShader;
        TQRTextures        m_Textures;
        TQRMatrix4x4       m_ProjectionMatrix;
        TQRMatrix4x4       m_ViewMatrix;
        TQRMatrix4x4       m_ModelMatrix;
        std::time_t        m_PreviousTime;
        double             m_InterpolationFactor;
        std::size_t        m_FrameIndex;

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
        *@param fullScreen - if true, application will be opened in full screen mode
        *@param useShader - if true, shader will be used
        *@param collisions - if true, collisions are detected and visible
        *@return true on success, otherwise false
        */
        bool LoadModel(bool fullScreen, bool useShader, bool collisions);

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
        *@param useShader - whether or not shader are used to draw model
        *@param collisions - whether or not collisions are visible
        */
        void DetectAndDrawCollisions(const TQRMatrix4x4& modelMatrix,
                                           TQRAABBTree*  pAABBTree,
                                           bool          useShader,
                                           bool          collisions);

        /**
        * Prepres the shader to draw the model
        *@param pShader - shader to prepare
        *@param textures - textures belonging to model
        */
        void PrepareShaderToDrawModel(QR_Shader_OpenGL* pShader, const TQRTextures& textures);

        /**
        * Loads model texture
        *@param pTexture - texture info
        *@return true on success, otherwise false
        */
        bool LoadTexture(TQRTexture* pTexture);

        /**
        * Draws model
        *@param pGroup - group at which model belongs
        *@param pModel - model to draw
        *@param textures - textures belonging to model, in the order where they should be combined
        *@param matrix - model matrix
        *@param index - model mesh index
        *@param nextIndex - model mesh index to interpolate with
        *@param interpolationFactor - interpolation factor
        *@param useShader - whether or not shader are used to draw model
        *@param collisions - whether or not collisions are visible
        */
        void DrawModel(TQRMD3Model*         pModel,
                       const TQRTextures    textures,
                       const TQRMatrix4x4&  matrix,
                       NativeInt            index,
                       NativeInt            nextIndex,
                       const double         interpolationFactor,
                       bool                 useShader,
                       bool                 collisions);
};
extern PACKAGE TMainForm* MainForm;
#endif
