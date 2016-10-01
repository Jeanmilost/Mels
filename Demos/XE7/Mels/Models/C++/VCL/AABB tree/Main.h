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
#include <UTQRShapes.hpp>

// OpenGL
#include <gl\gl.h>

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
        HDC          m_hDC;
        HGLRC        m_hRC;
        TQRMesh      m_Mesh;
        TQRAABBTree* m_pAABBTree;
        std::time_t  m_PreviousTime;
        std::size_t  m_CollidePolygonsCount;
        std::size_t  m_HighestHit;
        float        m_Theta;
        bool         m_Rotate;

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
        * Creates a sphere
        *@param radius - sphere radius
        *@param slices - slices (longitude) number
        *@param stacks - stacks (latitude) number
        *@param color - color in RGBA format
        *@param vertex - vertex format to use to generate mesh
        *@param[out] mesh - mesh representing sphere
        */
        void CreateSphere(float       radius,
                          int         slices,
                          int         stacks,
                          std::size_t color,
                          TQRVertex&  vertex,
                          TQRMesh&    mesh);
};
extern PACKAGE TMainForm* MainForm;
#endif

