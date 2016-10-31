#ifndef MainH
#define MainH

// vcl
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Imaging.pngimage.hpp>

// mels
#include <UTQRVCLModelComponentGL.hpp>
#include <UTQRVCLShapeComponentGL.hpp>

class TMainForm : public TForm
{
__published:	// IDE-managed Components
    TPageControl *pcModels;
    TTabSheet *tsSurface;
    TPanel *paSurfaceLeft;
    TQRVCLSurfaceGL *suSurface;
private:	// User declarations
public:		// User declarations
    __fastcall TMainForm(TComponent* Owner);
};
extern PACKAGE TMainForm *MainForm;
#endif
