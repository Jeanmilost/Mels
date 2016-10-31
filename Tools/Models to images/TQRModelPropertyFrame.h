//---------------------------------------------------------------------------

#ifndef TQRModelPropertyFrameH
#define TQRModelPropertyFrameH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
//---------------------------------------------------------------------------
class TQRModelPropertyFrame : public TFrame
{
__published:	// IDE-managed Components
    TGroupBox *gbTexture;
    TLabel *laTextureFileName;
    TPanel *paTextureFileName;
    TButton *btTextureFileNameBrowse;
    TGroupBox *gbAlphaBlending;
    TCheckBox *ckEnabled;
    TPanel *paAlphaBlendingGlobalLevel;
    TEdit *edAlphaBlendingGlobalLevel;
    TUpDown *udAlphaBlendingGlobalLevel;
    TLabel *laAlphaBlendingGlobalLevel;
    TLabel *laAntialiasing;
    TComboBox *cbAntialiasing;
    TGroupBox *gbColors;
    TPanel *paColorsBackground;
    TLabel *laColorsBackground;
    TPanel *paColorsBackgroundColor;
    TLabel *laColorsModel;
    TPanel *paColorsModelColor;
    TGroupBox *gbModelPosition;
    TLabel *laModelPositionPosition;
    TPanel *paModelPositionPosition;
    TEdit *edModelPositionPositionX;
    TLabel *laModelPositionPositionX;
    TLabel *laModelPositionPositionY;
    TEdit *edModelPositionPositionY;
    TLabel *laModelPositionPositionZ;
    TEdit *edModelPositionPositionZ;
    TLabel *laModelPositionRotation;
    TPanel *paModelPositionRotation;
    TLabel *laModelPositionRotationX;
    TLabel *laModelPositionRotationY;
    TLabel *laModelPositionRotationZ;
    TEdit *edModelPositionRotationX;
    TEdit *edModelPositionRotationY;
    TEdit *edModelPositionRotationZ;
    TLabel *laModelPositionScaling;
    TPanel *paModelPositionScaling;
    TLabel *laModelPositionScalingX;
    TLabel *laModelPositionScalingY;
    TLabel *laModelPositionScalingZ;
    TEdit *edModelPositionScalingX;
    TEdit *edModelPositionScalingY;
    TEdit *edModelPositionScalingZ;
    TGroupBox *gbModelOptions;
private:	// User declarations
public:		// User declarations
    __fastcall TQRModelPropertyFrame(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TQRModelPropertyFrame *QRModelPropertyFrame;
//---------------------------------------------------------------------------
#endif
