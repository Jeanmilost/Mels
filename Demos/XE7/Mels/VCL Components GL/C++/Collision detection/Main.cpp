// *************************************************************************************************
// * ==> Main -------------------------------------------------------------------------------------*
// *************************************************************************************************
// * MIT License - The Mels Library, a free and easy-to-use 3D Models library                      *
// *                                                                                               *
// * Permission is hereby granted, free of charge, to any person obtaining a copy of this software *
// * and associated documentation files (the "Software"), to deal in the Software without          *
// * restriction, including without limitation the rights to use, copy, modify, merge, publish,    *
// * distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the *
// * Software is furnished to do so, subject to the following conditions:                          *
// *                                                                                               *
// * The above copyright notice and this permission notice shall be included in all copies or      *
// * substantial portions of the Software.                                                         *
// *                                                                                               *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING *
// * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND    *
// * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,  *
// * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      *
// * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *
// *************************************************************************************************

#include <vcl.h>
#pragma hdrstop
#include "Main.h"

// std
#include <memory>

// opengl
#include <gl/gl.h>

#pragma package(smart_init)
#pragma link "UTQRVCLModelComponentGL"
#pragma link "UTQRVCLMD2ModelComponentGL"
#pragma resource "*.dfm"

TMainForm *MainForm;
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner) :
    TForm(Owner)
{}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::m2ModelDetectCollisions(TObject* pSender,
                                        const TQRMatrix4x4& projectionMatrix,
                                        const TQRMatrix4x4& viewMatrix,
                                        const TQRMatrix4x4& modelMatrix,
                                               TQRAABBTree* pAABBTree,
                                     TQRVCLModelRendererGL* pRenderer,
                                       TQRVCLModelShaderGL* pShader)
{
    DetectAndDrawCollisions(projectionMatrix, viewMatrix, modelMatrix, pAABBTree, pRenderer, pShader);
}
//--------------------------------------------------------------------------------------------------
void TMainForm::DetectAndDrawCollisions(const TQRMatrix4x4& projectionMatrix,
                                        const TQRMatrix4x4& viewMatrix,
                                        const TQRMatrix4x4& modelMatrix,
                                               TQRAABBTree* pAABBTree,
                                     TQRVCLModelRendererGL* pRenderer,
                                       TQRVCLModelShaderGL* pShader)
{
    if (!pAABBTree)
        return;

    // calculate client rect in OpenGL coordinates
    TQRRect rect(-1.0, 1.0, 2.0, 2.0);

    // convert mouse position to OpenGL point, that will be used as ray start pos, and create ray dir
    TQRVector3D rayPos = pRenderer->MousePosToGLPoint((NativeUInt)Handle, rect);
    TQRVector3D rayDir(0.0, 0.0, 1.0);

    // move the ray to match with the model coordinates
    rayPos.Y += 0.05f;

    // this is a lazy way to correct a perspective issue. In fact, the model is much larger than its
    // image on the screen, but it is placed very far in relation to the screen. In the model
    // coordinates, the ray location is beyond the mouse coordinate. For that, a ratio is needed to
    // keep the ray coordinates coherent with the mouse position. Not ideal (e.g. the model feet are
    // not always well detected), but this is efficient for the majority of cases
    rayPos.MulAndAssign(1.1f);

    float determinant;

    // transform the ray to be on the same coordinates system as the model
    TQRMatrix4x4 invertProj   = const_cast<TQRMatrix4x4&>(projectionMatrix).Inverse(determinant);
    TQRMatrix4x4 invertView   = const_cast<TQRMatrix4x4&>(viewMatrix).Inverse(determinant);
    TQRMatrix4x4 invertModel  = const_cast<TQRMatrix4x4&>(modelMatrix).Inverse(determinant);
    TQRMatrix4x4 invertMatrix = invertProj.Multiply(invertView.Multiply(invertModel));
    rayPos                    = invertMatrix.Transform(rayPos);
    rayDir                    = invertMatrix.Transform(rayDir);

    // create and populate ray from mouse position
    std::auto_ptr<TQRRay> pRay(new TQRRay());
    pRay->Pos = &rayPos;
    pRay->Dir = &rayDir;

    TQRPolygons polygons;

    // get polygons to check for collision by resolving AABB tree
    pAABBTree->Resolve(pRay.get(), polygons);

    TQRPolygons polygonToDraw;
    std::size_t polygonCount = polygons.Length;

    // iterate through polygons to check
    if (polygonCount > 0)
        for (std::size_t i = 0; i < polygonCount; ++i)
            // is polygon intersecting ray?
            if (TQRCollisionHelper::GetRayPolygonCollision(pRay.get(), polygons[i]))
            {
                // ad polygon in collision to resulting list
                polygonToDraw.Length                    += 1;
                polygonToDraw[polygonToDraw.Length - 1]  = polygons[i];
            }

    std::size_t polygonToDrawCount = polygonToDraw.Length;

    // found polgons to draw?
    if (!polygonToDrawCount)
        return;

    TQRMesh mesh;

    try
    {
        // create a vertex buffer and populate it with polygons in collisions
        mesh.Length             = 1;
        mesh[0].m_Type          = EQR_VT_Triangles;
        mesh[0].m_CoordType     = EQR_VC_XYZ;
        mesh[0].m_Stride        = 7;
        mesh[0].m_Buffer.Length = polygonToDrawCount * (mesh[0].m_Stride * 3);
        mesh[0].m_Format << EQR_VF_Colors;

        std::size_t offset = 0;

        // iterate through polygons to draw
        for (std::size_t i = 0; i < polygonToDrawCount; ++i)
        {
            // build polygon to show
            mesh[0].m_Buffer[offset]      = polygonToDraw[i].Vertex1->X;
            mesh[0].m_Buffer[offset + 1]  = polygonToDraw[i].Vertex1->Y;
            mesh[0].m_Buffer[offset + 2]  = polygonToDraw[i].Vertex1->Z;
            mesh[0].m_Buffer[offset + 3]  = 1.0f;
            mesh[0].m_Buffer[offset + 4]  = 0.0f;
            mesh[0].m_Buffer[offset + 5]  = 0.0f;
            mesh[0].m_Buffer[offset + 6]  = 1.0f;
            mesh[0].m_Buffer[offset + 7]  = polygonToDraw[i].Vertex2->X;
            mesh[0].m_Buffer[offset + 8]  = polygonToDraw[i].Vertex2->Y;
            mesh[0].m_Buffer[offset + 9]  = polygonToDraw[i].Vertex2->Z;
            mesh[0].m_Buffer[offset + 10] = 0.8f;
            mesh[0].m_Buffer[offset + 11] = 0.0f;
            mesh[0].m_Buffer[offset + 12] = 0.2f;
            mesh[0].m_Buffer[offset + 13] = 1.0f;
            mesh[0].m_Buffer[offset + 14] = polygonToDraw[i].Vertex3->X;
            mesh[0].m_Buffer[offset + 15] = polygonToDraw[i].Vertex3->Y;
            mesh[0].m_Buffer[offset + 16] = polygonToDraw[i].Vertex3->Z;
            mesh[0].m_Buffer[offset + 17] = 1.0f;
            mesh[0].m_Buffer[offset + 18] = 0.12f;
            mesh[0].m_Buffer[offset + 19] = 0.2f;
            mesh[0].m_Buffer[offset + 20] = 1.0f;

            // go to next polygon
            offset += (mesh[0].m_Stride * 3);
        }

        glMatrixMode(GL_MODELVIEW);

        glPushMatrix();

        // place triangles into 3D world
        glLoadMatrixf(PGLfloat(const_cast<TQRMatrix4x4&>(modelMatrix).GetPtr()));

        // configure OpenGL to draw polygons in collision
        glDisable(GL_TEXTURE_2D);
        glCullFace(GL_NONE);
        glDisable(GL_DEPTH_TEST);

        TQRTextures textures;

        // draw polygons in collision with mouse pointer
        pRenderer->Draw(mesh, modelMatrix, textures);

        // restore previous OpenGL parameters
        glEnable(GL_DEPTH_TEST);
        glCullFace(GL_FRONT);
        glEnable(GL_TEXTURE_2D);

        glPopMatrix();

        glFlush();
    }
    __finally
    {
        if (mesh.Length)
        {
            mesh[0].m_Buffer.Length = 0;
            mesh.Length             = 0;
        }
    }
}
//--------------------------------------------------------------------------------------------------
