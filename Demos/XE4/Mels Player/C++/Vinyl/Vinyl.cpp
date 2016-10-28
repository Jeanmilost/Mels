/**************************************************************************************************
 * ==> Vinyl -------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Vinyl demo main entry point                                                      *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop
#pragma package(smart_init)

//--------------------------------------------------------------------------------------------------
USEFORM("Main.cpp", MainForm);
//--------------------------------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
    try
    {
         Application->Initialize();
         Application->MainFormOnTaskBar = true;
         Application->CreateForm(__classid(TMainForm), &MainForm);
         Application->Run();
    }
    catch (Exception &exception)
    {
         Application->ShowException(&exception);
    }
    catch (...)
    {
         try
         {
             throw Exception("");
         }
         catch (Exception &exception)
         {
             Application->ShowException(&exception);
         }
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------
