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
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Imaging.jpeg.hpp>

// mels
#include <UTQR3D.hpp>
#include <UTQRVCLModelComponentGL.hpp>
#include <UTQRVCLShapeComponentGL.hpp>

class TMainForm : public TForm
{
    __published:	// IDE-managed Components
        TScrollBox *sbMain;
        TPanel *paMercury;
        TQRVCLSphereGL *spMercury;
        TPanel *paMercuryMain;
        TLabel *laMercury;
        TLabel *laMercuryDesc;
        TPanel *paVenus;
        TQRVCLSphereGL *spVenus;
        TPanel *paVenusMain;
        TLabel *laVenus;
        TLabel *laVenusDesc;
        TPanel *paEarth;
        TQRVCLSphereGL *spEarth;
        TPanel *paEarthMain;
        TLabel *laEarth;
        TLabel *laEarthDesc;
        TPanel *paMars;
        TQRVCLSphereGL *spMars;
        TPanel *paMarsMain;
        TLabel *laMain;
        TLabel *laMarsDesc;
        TPanel *paMoon;
        TQRVCLSphereGL *spMoon;
        TPanel *paMoonMain;
        TLabel *laMoon;
        TLabel *laMoonDesc;
        TPanel *paJupiter;
        TQRVCLSphereGL *spJupiter;
        TPanel *paJupiterMain;
        TLabel *laJupiter;
        TLabel *laJupiterDesc;
        TPanel *paSaturn;
        TQRVCLSphereGL *spSaturn;
        TPanel *paSaturnMain;
        TLabel *laSaturn;
        TLabel *laSaturnDesc;
        TPanel *paUranus;
        TQRVCLSphereGL *spUranus;
        TPanel *paUranusMain;
        TLabel *laUranus;
        TLabel *laUranusDesc;
        TPanel *paNeptune;
        TQRVCLSphereGL *spNeptune;
        TPanel *paNeptuneMain;
        TLabel *laNeptune;
        TLabel *laNeptuneDesc;
        TTimer *tiAnimation;

        void __fastcall spSaturnAfterDrawScene(TObject*               pSender,
                                               NativeUInt             hDC,
                                               NativeUInt             hGLRC,
                                               TQRVCLModelRendererGL* pRenderer,
                                               TQRVCLModelShaderGL*   pShader);
        bool __fastcall spSaturnCreateSceneMatrix(TObject*               pSender,
                                                  TQRMatrix4x4&          projectionMatrix,
                                                  TQRMatrix4x4&          viewMatrix,
                                                  NativeUInt             hDC,
                                                  NativeUInt             hGLRC,
                                                  TQRVCLModelRendererGL* pRenderer,
                                                  TQRVCLModelShaderGL*   pShader);
        void __fastcall spSaturnLoadTexture(TObject*               pSender,
                                            NativeUInt             hDC,
                                            NativeUInt             hGLRC,
                                            TQRVCLModelRendererGL* pRenderer,
                                            TQRVCLModelShaderGL*   pShader);
        void __fastcall tiAnimationTimer(TObject* pSender);

    public:
        /**
        * Constructor
        *@param pOwner - form owner
        */
        __fastcall TMainForm(TComponent* pOwner);

        /**
        * Destructor
        */
        __fastcall ~TMainForm();

    private:
        TQRMesh     m_Rings;
        TQRTextures m_RingTextures;
        float       m_Angle;

        /**
        * Generates a ring
        *@param slices - number of slices composing the ring)
        *@param innerRadius - inner radius
        *@param outerRadius - outer radius
        *@param[out] mesh - generated ring mesh
        */
        void GenerateRing(std::size_t slices,
                          float       innerRadius,
                          float       outerRadius,
                          TQRMesh&    mesh);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm* MainForm;
//---------------------------------------------------------------------------
#endif
