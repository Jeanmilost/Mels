/**************************************************************************************************
 * ==> Main --------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : 3D ray picking with AABB tree simplification demo main form                      *
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
#include <Vcl.ExtCtrls.hpp>
#include <System.Actions.hpp>
#include <Vcl.ActnList.hpp>

// std
#include <time.h>

// Mels library
#include <UTQRCollision.hpp>
#include <UTQRShapeGroup.hpp>

// OpenGL
#include <gl\gl.h>
#include <gl\glu.h>

/**
* 3D ray picking with AABB simplification demo main form
*@author Jean-Milost Reymond
*/
class TMainForm : public TForm
{
    __published:
        TPopupMenu *pmOptions;
        TMenuItem *miRotateSphere;
        TPanel *paInfo;
        TLabel *laTotal;
        TLabel *laToTest;
        TLabel *laInCollision;
        TButton *btRotate;
        TActionList *alRotate;
        TAction *acRotate;
        TPanel *paCollisionResult;
        TLabel *laHighestHit;

        void __fastcall FormPaint(TObject* pSender);
        void __fastcall acRotateExecute(TObject* pSender);
        void __fastcall FormShow(TObject* pSender);

    public:
        /**
        * Constructor
        *@param pOwner - form owner
        */
        __fastcall TMainForm(TComponent* Owner);

        /**
        * Destructor
        */
        virtual __fastcall ~TMainForm();

    protected:
        /**
        * Prepares and draws OpenGL scene
        */
        void __fastcall RenderGLScene();

        /**
        * Draws scene
        *@param elapsedTime - elapsed time since last draw
        */
        void __fastcall DrawScene(const double& elapsedTime);

        /**
        * Called when application do nothing
        *@param pSender - event sender
        *@param[in, out] done - if true, loop will be considered as completed
        */
        void __fastcall OnIdle(TObject* pSender, bool& done);

    private:
        HDC             m_hDC;
        HGLRC           m_hRC;
        TQRSphereGroup* m_pSphere;
        TQRMesh         m_Mesh;
        TQRAABBTree*    m_pAABBTree;
        std::time_t     m_PreviousTime;
        std::size_t     m_CollidePolygonsCount;
        std::size_t     m_HighestHit;
        float           m_Theta;
        bool            m_Rotate;

        /**
        * Shows collision detection status
        *@param toTest - polygons to test count
        *@aram inCollision - polygons in collision count
        */
        void ShowStatus(int toTest, int inCollision) const;

        /**
        * Configures OpenGL
        */
        void ConfigOpenGL();

        /**
        * Called when static model will be drawn and caller should extract mesh
        *@param pGroup - group at which model belongs
        *@param pModel - model to draw
        *@param textures - texture list, in order they should be linked
        *@param matrix - model matrix
        */
        void __fastcall OnDrawCustomStaticModelItem(TQRModelGroup* const pGroup,
                                                    TQRModel*            pModel,
                                                    const TQRTextures    textures,
                                                    const TQRMatrix4x4&  matrix);
};
extern PACKAGE TMainForm* MainForm;
#endif
