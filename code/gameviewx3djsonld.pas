{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewX3DJSONLD;

interface

uses Classes, CastleWindow, CastleDownload,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,
  X3DNodes, SysUtils, CastleScene, X3DLoad, CastleLog,
  CastleUriUtils, CastleStringUtils,
  X3DJSONLDX3DNode;

type
  { Main view, where most of the application logic takes place. }
  TViewX3DJSONLD = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    procedure SetUpJSON();
    procedure SetScene(scene: TCastleScene);
  end;

var
  ViewX3DJSONLD: TViewX3DJSONLD;
  Scene: TCastleScene;
  loader: TX3DJSONLDX3DNode;

implementation

{ TViewX3DJSONLD ----------------------------------------------------------------- }
    
procedure DropJsonFiles(Sender: TCastleContainer; const FileNames: array of string);
var
  I: LongInt;
  Count: LongInt;
  Url: string;
begin
  WritelnLog('DEBUG', '---------------------------------');
  Count := Length(FileNames);
  WritelnLog('DEBUG', 'Files dropped: %d', [Count]);
  for I := 0 to Count - 1 do
  begin
    WritelnLog('DEBUG', '- File %d: %s', [I, FileNames[I]]);
    Url := FilenameToUriSafe(FileNames[I]);
    LoadNode(Url);
    WritelnLog('url is '+ SReadableForm(Url));
    Scene.Load(Url);
  end;
  WritelnLog('DEBUG', '---------------------------------');
end;

constructor TViewX3DJSONLD.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Application.MainWindow.OnDropFiles := @DropJsonFiles;
  DesignUrl := 'castle-data:/gameviewx3djsonld.castle-user-interface';
  SetUpJSON();
end;

procedure TViewX3DJSONLD.SetUpJSON;
begin
  loader := TX3DJSONLDX3DNode.Create;
  loader.RegisterJSON();
end;

procedure TViewX3DJSONLD.SetScene(scene: TCastleScene);
begin
  Scene := scene;
end;

procedure TViewX3DJSONLD.Start;
begin
  inherited;
end;

procedure TViewX3DJSONLD.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewX3DJSONLD.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewX3DJSONLD.Press method should be used to handle keys
    not handled in children controls.
  }

  // Use this to handle keys:
  {
  if Event.IsKey(keyXxx) then
  begin
    // DoSomething;
    Exit(true); // key was handled
  end;
  }
end;

end.
