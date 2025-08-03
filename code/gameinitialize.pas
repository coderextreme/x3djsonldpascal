{ Game initialization.
  This unit is cross-platform.
  It will be used by the platform-specific program or library file.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameInitialize;

interface

implementation

uses
  SysUtils, CastlePasJSON,
  // List the engine units first:
  CastleWindow, CastleLog, CastleUIControls, X3DFields, DOM,
  X3DNodes, X3DLoad, CastleScene, CastleViewport, CastleUriUtils, CastleStringUtils,
  // THEN list your unit that depends on them:
  X3DJSONLDX3DNode,
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  GameViewX3DJSONLD
  {$endregion 'Castle Initialization Uses'};

var
  Window: TCastleWindow;
  loader: TX3DJSONLDX3DNode;
  jsonitem: TPasJSONItem;  // top level item is a JSON object, below
  jsonobj: TPasJSONItemObject;
  document: TDOMDocument;
  xmlOutput: String;
  Scene: TCastleScene;


{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Create views (see https://castle-engine.io/views ). }
  {$region 'Castle View Creation'}
  // The content here may be automatically updated by CGE editor.
  ViewX3DJSONLD := TViewX3DJSONLD.Create(Application);
  {$endregion 'Castle View Creation'}
  Scene := TCastleScene.Create(Application);

  Window.Container.View := ViewX3DJSONLD;
  Window.Controls.InsertFront(ViewX3DJSONLD);
  // Java
  loader := TX3DJSONLDX3DNode.Create;
  try
    jsonitem := loader.ReadJsonFile('C:\Users\jcarl\X3DJSONLD\src\main\data\ArchHalf.json');
    if jsonitem is TPasJSONItemObject then
      begin
        jsonobj := TPasJSONItemObject(jsonitem);
        document := loader.LoadJsonIntoDocument(jsonobj, loader./gerFieX3DVersion(jsonobj), False);
        xmlOutput := loader.SerializeDOM(loader.GetX3DVersion(jsonobj), document);
        WriteLnLog('DEBUG', xmlOutput);
  	ViewX3DJSONLD.SetXmlOutput(xmlOutput, Scene);
      end
    else
      begin
        WriteLnLog('ERROR', 'Not a JSON Object; an X3D JSON Document is a JSON Object.');
      end;
  finally
    loader.Free;
  end;
end;




initialization
  { This initialization section configures:
    - Application.OnInitialize
    - Application.MainWindow
    - determines initial window size

    You should not need to do anything more in this initialization section.
    Most of your actual application initialization (in particular, any file reading)
    should happen inside ApplicationInitialize. }

  Application.OnInitialize := @ApplicationInitialize;

  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

  { Optionally, adjust window fullscreen state and size at this point.
    Examples:

    Run fullscreen:

      Window.FullScreen := true;

    Run in a 600x400 window:

      Window.FullScreen := false; // default
      Window.Width := 600;
      Window.Height := 400;

    Run in a window taking 2/3 of screen (width and height):

      Window.FullScreen := false; // default
      Window.Width := Application.ScreenWidth * 2 div 3;
      Window.Height := Application.ScreenHeight * 2 div 3;

    Note that some platforms (like mobile) ignore these window sizes.
  }

  { Handle command-line parameters like --fullscreen and --window.
    By doing this last, you let user to override your fullscreen / mode setup. }
  Window.ParseParameters;
end.
