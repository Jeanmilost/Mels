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

#ifndef MainH
#define MainH

// vcl
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>

// mels
#include <UTQRVCLModelComponentGL.hpp>
#include <UTQRVCLMD2ModelComponentGL.hpp>

/**
* Main form class
*/
class TMainForm : public TForm
{
    __published:
        TQRVCLMD2ModelGL *m2Model;

        void __fastcall m2ModelDetectCollisions(TObject* pSender,
                                     const TQRMatrix4x4& projectionMatrix,
                                     const TQRMatrix4x4& modelMatrix,
                                            TQRAABBTree* pAABBTree,
                                  TQRVCLModelRendererGL* pRenderer,
                                    TQRVCLModelShaderGL* pShader);

    public:
        __fastcall TMainForm(TComponent* pOwner);

    private:
        /**
        * Detect collisions between the mouse pointer and the model and draw polyons in collision
        *@param pSender - event sender
        *@param projectionMatrix - projection (or word) matrix used to render the model
        *@param modelMatrix - model matrix
        *@param pAABBTree - model aligned-axis bounding box tree
        *@param pRenderer - openGL renderer
        *@param pShader - openGL shader
        */
        void DetectAndDrawCollisions(const TQRMatrix4x4& projectionMatrix,
                                     const TQRMatrix4x4& modelMatrix,
                                            TQRAABBTree* pAABBTree,
                                  TQRVCLModelRendererGL* pRenderer,
                                    TQRVCLModelShaderGL* pShader);
};
extern PACKAGE TMainForm* MainForm;
#endif
