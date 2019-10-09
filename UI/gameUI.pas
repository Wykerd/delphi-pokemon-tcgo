unit gameUI;

interface
// Page 148
uses
  Windows, Classes, Forms, Dialogs, StdCtrls, Graphics, SysUtils, helpers, System.JSON,
  Controls, ExtCtrls, UIContainer, OpenGL, Textures, pkmCard, clientState, System.threading,
  IdGlobal, idhash, IdHashSHA, gameActionUI, idIohandler;

type
  TChatEvent = procedure (s : string) of object;

  TPickContext = class (TObject)
  published
    constructor Create(_index: integer; _model: TCardModel; found_in: string); overload;
  public
    Index: integer;
    Model: TCardModel;
    FoundIn: string;
  end;

  TGameUI = class (TUIContainer)
  private
    last_render_ticks : integer;
    FOnChat: TChatEvent;
    FPanAngleX, FPanAngleY: Extended;
    FLastState : TJSONObject;
    last_pick : integer;
    FRenderCache: TArray<TCardModel>;
    FBoardTex, FEdgeTex, FBenchEdgeTex, FBenchTex, FFontTexture : GLuint;
    ActiveUI : TActiveUI;
    HandUI : THandUI;
    BenchUI: TBenchUI;
    FIOhandler: TIdIOHandler;
    FCredentials: TJSONPair;
    procedure SetOnChat(const Value: TChatEvent);
    procedure OpenChat(sender: Tobject);
    procedure HandleResize(Sender: TObject);
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HandleKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure HandleCardClick(Context: TPickContext);
    procedure SetCredentials(const Value: TJSONPair);
    procedure SetIOhandler(const Value: TIdIOHandler);
  published
    constructor Create (AOwner: TComponent); override;
    // Event Handlers //
    property OnChat : TChatEvent read FOnChat write SetOnChat;
    property IOhandler: TIdIOHandler read FIOhandler write SetIOhandler;
    property Credentials: TJSONPair read FCredentials write SetCredentials;
    // -- //
    // OpenGL //
    // Rendering cycle //
    procedure Pick (X, Y: Integer);
    procedure Paint; override;
    procedure Draw;
    procedure Init;
    procedure Update;
    procedure RenderState(state: TJSONObject);
    procedure PopulateArray (var _fill: TArray<integer>; prop: string; plr_state: TJSONObject);
    procedure LocateInCache(json: string; callback: TProc<Boolean, Integer>);
    // -- //
    // Handlers //
    procedure IncomingChat (s : string);
    // -- //
  public
    // Rendering cycle variables //
    // State //
    p_deck : integer;
    p_deck_length: byte;
    p_prize_cards: byte;
    p_benched_cards: TArray<integer>;
    p_hand: TArray<integer>;
    p_discard: TArray<integer>;
    p_active: integer;

    o_deck_length : byte;
    o_prize_cards: byte;
    o_benched_cards: TArray<integer>;
    o_hand: byte;
    o_discard: TArray<integer>;
    o_active: integer;

    turn: boolean;
    stage: string;

    prohibit_render : boolean;
    // -- //
  end;

var
  debugTex : GLuint;

implementation

{ GL Setup Functions }
procedure setupPixelFormat(DC: HDC);
const
  pfd: TPIXELFORMATDESCRIPTOR = (
    nSize: sizeof(TPIXELFORMATDESCRIPTOR); // size
    nVersion: 1; // version
    dwFlags: PFD_SUPPORT_OPENGL or PFD_DRAW_TO_WINDOW or PFD_DOUBLEBUFFER;
    // support double-buffering
    iPixelType: PFD_TYPE_RGBA; // color type
    cColorBits: 24; // preferred color depth
    cRedBits: 0; cRedShift: 0; // color bits (ignored)
    cGreenBits: 0; cGreenShift: 0; cBlueBits: 0; cBlueShift: 0; cAlphaBits: 0;
    cAlphaShift: 0; // no alpha buffer
    cAccumBits: 0; cAccumRedBits: 0; // no accumulation buffer,
    cAccumGreenBits: 0; // accum bits (ignored)
    cAccumBlueBits: 0; cAccumAlphaBits: 0; cDepthBits: 16; // depth buffer
    cStencilBits: 0; // no stencil buffer
    cAuxBuffers: 0; // no auxiliary buffers
    iLayerType: PFD_MAIN_PLANE; // main layer
    bReserved: 0; dwLayerMask: 0; dwVisibleMask: 0; dwDamageMask: 0;
    // no layer, visible, damage masks
  );
var
  pixelFormat: Integer;
begin
  pixelFormat := ChoosePixelFormat(DC, @pfd);
  if (pixelFormat = 0) then
    exit;
  if (SetPixelFormat(DC, pixelFormat, @pfd) <> TRUE) then
    exit;
end;

procedure GLInit;
begin
  // set viewing projection
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(45.0, 16 / 9, 2.0, 25.0);
  // glFrustum(-1.6, 1.6, -0.9, 0.9, 2.0, 25.0);
  // position viewer
  glMatrixMode(GL_MODELVIEW);

  // Enable GL capabilities
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_NORMALIZE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GREATER, 0.4);
  // Default textures to enabled
  glEnable(GL_TEXTURE_2D);
end;

{ TGameUI }

constructor TGameUI.Create(AOwner: TComponent);
begin
  inherited;
  SetLength(FRenderCache, 0);
  prohibit_render := false;
  OnKeyDown := HandleKeyDown;
  OnClick := OpenChat;
  OnMouseMove := HandleMouseMove;
  OnMouseDown := HandleMouseDown;
  OnResize := HandleResize;
  RenderState(TJSONObject.Create);

  ActiveUI := TActiveUI.Create(self);
  ActiveUI.Visible := false;
  ActiveUI.Parent := AOwner as TWinControl;

  HandUI := THandUI.Create(self);
  HandUI.Visible := false;
  HandUI.Parent := AOwner as TWinControl;;
  HandUI.RenderCache := FRenderCache;
  HandUI.Benched := p_benched_cards;

  ActiveUI.Benched := p_benched_cards;

  BenchUI := TBenchUI.Create(self);
  BenchUI.Parent := AOwner as TWinControl;;
  BenchUI.Visible := false;
end;

procedure TGameUI.Init;
var
  DC: HDC;
  RC: HGLRC;
  i: Integer;
begin
  // Setup OpenGL
  DC := GetDC(Self.Handle);
  setupPixelFormat(DC);
  RC := wglCreateContext(DC);
  wglMakeCurrent(DC, RC); // makes OpenGL window active
  GLInit; // initialize OpenGL

  // Load the static textures
  LoadTexture('boardTexture.jpg', FBoardTex, true);
  LoadTexture('edgeTexture.jpg', FEdgeTex, true);
  LoadTexture('greyEdgeTexture.jpg', FBenchEdgeTex, true);
  LoadTexture('benchTexture.jpg', FBenchTex, true);
  LoadTexture('fontTexture.tga', FFontTexture, true);
  LoadPkmModelTextures;
end;

procedure TGameUI.LocateInCache(json: string;
  callback: TProc<Boolean, Integer>);
var
  I: Integer;
  hash: string;
  hasher : TIdHashSHA1;
begin
  hasher := TIdHashSHA1.Create;
  with hasher do
  begin
    hash := HashStringAsHex(json);
  end;
  hasher.Free;
  for I := 0 to High(FRenderCache) do
  begin
    if FRenderCache[i].Sprite.hash = hash then
    begin
      callback(true, i);
      exit;
    end;
  end;
  callback(false, -1);
end;

procedure TGameUI.Draw;
var
  last_index: Integer;
  procedure RenderHandArr (arr : TArray<integer>; x_offset, y_offset, z_offset: Extended);
  var
    i: Integer;
  begin
    if arr <> nil then
    if length(arr) <> 0 then
    begin
      for I := 0 to Length(arr) - 1 do
      begin
        if (arr[i] > -1) then
        begin
          glPushMatrix;
          glPushName(i + 1 + last_index);
          glTranslatef(0.0, 1.5, -10.0);
          glRotate(FPanAngleX, 0, 1, 0);
          glRotate(FPanAngleY, 1, 0, 0);
          glRotate(315, 1, 0, 0);
          if Length(arr) <> 1 then
            glTranslatef(x_offset + (((7-0.57)/(Length(arr) - 1)) * i), y_offset, 0.1)
          else
            glTranslatef(x_offset, y_offset, 0.1);
          if i + 1 + last_index = last_pick then
          begin
            glTranslatef(0, 0, 0.2);
            glRotate(20, 1, 0, 0);
          end;
            FRenderCache[arr[i]].Draw;
            FRenderCache[arr[i]].name := i + 1 + last_index;
          glPopName;
          glPopMatrix;
        end;
      end;
      last_index := last_index + length(arr);
    end;
  end;

  procedure RenderOpHandArr (length : byte; x_offset, y_offset, z_offset: Extended);
  var
    i: Integer;
  begin
    for I := 0 to length - 1 do
    begin
      glPushMatrix;
      glTranslatef(0.0, 1.5, -10.0);
      glRotate(FPanAngleX, 0, 1, 0);
      glRotate(FPanAngleY, 1, 0, 0);
      glRotate(315, 1, 0, 0);
      if Length <> 1 then
        glTranslatef(x_offset + (((7-0.57)/(Length - 1)) * i)-7, y_offset, z_offset)
      else
        glTranslatef(x_offset - 7, y_offset, 0.1);
        TCardModel.RenderBackCard;
      glPopMatrix;
    end;
  end;

  procedure RenderDeck(length: integer; x, y: Extended; rotation : extended = 0);
  var
    i: integer;
  begin
    for I := 0 to length - 1 do
    begin
    glPushMatrix;
    glTranslatef(0.0, 1.5, -10.0);
    glRotate(FPanAngleX, 0, 1, 0);
    glRotate(FPanAngleY, 1, 0, 0);
    glRotate(315, 1, 0, 0);
    glTranslatef(x + (StaticRandom_1[i] * 0.05), y + (StaticRandom_1[59-i] * 0.05), i * 0.01);
    glRotate(10 * StaticRandom_2[i], 0, 0, 1);
    glRotate(rotation, 0, 0, 1);
      TCardModel.RenderBackCard;
    glPopMatrix;
    end;
  end;

  // Slightly diffirent usage of the static random numbers
  procedure RenderPrizes(length: integer; x, y: Extended; rotation : extended = 0);
  var
    i: integer;
  begin
    for I := 0 to length - 1 do
    begin
    glPushMatrix;
    glTranslatef(0.0, 1.5, -10.0);
    glRotate(FPanAngleX, 0, 1, 0);
    glRotate(FPanAngleY, 1, 0, 0);
    glRotate(315, 1, 0, 0);
    glTranslatef(x + (StaticRandom_2[i] * 0.05), y + (StaticRandom_2[59-i] * 0.05), i * 0.01);
    glRotate(10 * StaticRandom_1[i], 0, 0, 1);
    glRotate(rotation, 0, 0, 1);
      TCardModel.RenderBackCard;
    glPopMatrix;
    end;
    glPushMatrix;
    glTranslatef(0.0, 1.5, -10.0);
    glRotate(FPanAngleX, 0, 1, 0);
    glRotate(FPanAngleY, 1, 0, 0);
    glRotate(315, 1, 0, 0);
    glTranslate(x, y, 0.01);
    glScale(2, 2, 2);
    glBindTexture(GL_TEXTURE_2D, FFontTexture);
    glImgWrite(inttostr(length));
    glPopMatrix;
  end;

  procedure RenderDiscard(arr: TArray<integer>; x, y: Extended; rotation : extended = 0);
  var
    i: Integer;
  begin
    if arr <> nil then
    if length(arr) <> 0 then
    begin
      for I := 0 to Length(arr) - 1 do
      begin
        if (arr[i] > -1) then
        begin
          glPushMatrix;
          glTranslatef(0.0, 1.5, -10.0);
          glRotate(FPanAngleX, 0, 1, 0);
          glRotate(FPanAngleY, 1, 0, 0);
          glRotate(315, 1, 0, 0);
          glTranslatef(x + (StaticRandom_2[i] * 0.05),
            y + (StaticRandom_2[59-i] * 0.05), i * 0.01);
          glRotate(10 * StaticRandom_2[i], 0, 0, 1);
          glRotate(rotation, 0, 0, 1);
            FRenderCache[arr[i]].Draw;
          glPopMatrix;
        end;
      end;
    end;
  end;

  procedure RenderBench (arr : TArray<integer>; x_offset, y_offset: Extended);
  var
    i: Integer;
  begin
    if arr <> nil then
    if length(arr) <> 0 then
    begin
      for I := 0 to Length(arr) - 1 do
      begin
        if (arr[i] > -1) then
        begin
          glPushMatrix;
          glPushName(i + 1 + last_index);
          glTranslatef(0.0, 1.5, -10.0);
          glRotate(FPanAngleX, 0, 1, 0);
          glRotate(FPanAngleY, 1, 0, 0);
          glRotate(315, 1, 0, 0);
          if Length(arr) <> 1 then
            glTranslatef(x_offset + (((3.8-0.57)/(Length(arr) - 1)) * i), y_offset, 0.1)
          else
            glTranslatef(x_offset, y_offset, 0.1);
          if i + 1 + last_index = last_pick then
          begin
            glTranslatef(0, 0, 0.2);
            glRotate(20, 1, 0, 0);
          end;
            FRenderCache[arr[i]].Draw;
          glPopName;
          glPopMatrix;
        end;
      end;
      last_index := last_index + length(arr);
    end;
  end;

  procedure RenderOpBench (arr : TArray<integer>; x_offset, y_offset: Extended);
  var
    i: Integer;
  begin
    if arr <> nil then
    if length(arr) <> 0 then
    begin
      for I := 0 to Length(arr) - 1 do
      begin
        if (arr[i] > -1) then
        begin
          glPushMatrix;
          glTranslatef(0.0, 1.5, -10.0);
          glRotate(FPanAngleX, 0, 1, 0);
          glRotate(FPanAngleY, 1, 0, 0);
          glRotate(315, 1, 0, 0);
          glRotate(180, 0, 0, 1);
          if Length(arr) <> 1 then
            glTranslatef(x_offset + (((3.8-0.57)/(Length(arr) - 1)) * i), y_offset, 0.1)
          else
            glTranslatef(x_offset, y_offset, 0.1);
            FRenderCache[arr[i]].Draw;
          glPopMatrix;
        end;
      end;
    end;
  end;

begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;

  // Render the main board
  glPushMatrix;
  glPushName(1);
  glTranslatef(0.0, 1.5, -10.0);
  glRotate(FPanAngleX, 0, 1, 0);
  glRotate(FPanAngleY, 1, 0, 0);
  glRotate(315, 1, 0, 0);
    // Main board top
    glBindTexture(GL_TEXTURE_2D, FBoardTex);
    glBegin(GL_QUADS);
      glNormal3f(0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex3f(-3.5, -3.5, 0);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(3.5, -3.5, 0);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(3.5, 3.5, 0);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(-3.5, 3.5, 0);
    glEnd;

    // Main board edges
    glBindTexture(GL_TEXTURE_2D, FEdgeTex);
    glBegin(GL_QUADS);
      // Main board front
      glNormal3f(0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex3f(-3.5, -3.5, -0.1);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(3.5, -3.5, -0.1);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(3.5, -3.5, 0);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(-3.5, -3.5, 0);

      // Main board back
      glNormal3f(0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex3f(-3.5, 3.5, -0.1);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(3.5, 3.5, -0.1);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(3.5, 3.5, 0);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(-3.5, 3.5, 0);

      // Main board left
      glNormal3f(0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex3f(-3.5, -3.5, -0.1);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(-3.5, 3.5, -0.1);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(-3.5, 3.5, 0);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(-3.5, -3.5, 0);

      // Main board right
      glNormal3f(0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex3f(3.5, -3.5, -0.1);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(3.5, 3.5, -0.1);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(3.5, 3.5, 0);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(3.5, -3.5, 0);
    glEnd;

    // bench tops
    glBindTexture(GL_TEXTURE_2D, FBenchTex);
    glBegin(GL_QUADS);
      // Bottom
      glNormal3f(0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex3f(-2.0, -3.5, 0.1);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(2.0, -3.5, 0.1);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(2.0, -2.5, 0.1);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(-2.0, -2.5, 0.1);

      // Top
      glTexCoord2f(0.0, 0.0);
      glVertex3f(-2.0, 3.5, 0.1);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(2.0, 3.5, 0.1);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(2.0, 2.5, 0.1);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(-2.0, 2.5, 0.1);
    glEnd;

    // bench Edges
    glBindTexture(GL_TEXTURE_2D, FBenchEdgeTex);
    glBegin(GL_QUADS);
      // bottom bench front
      glNormal3f(0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex3f(-2.0, -3.5, 0.0);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(2.0, -3.5, 0.0);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(2.0, -3.5, 0.1);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(-2.0, -3.5, 0.1);

      // bottom bench back
      glNormal3f(0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex3f(-2.0, -2.5, 0.0);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(2.0, -2.5, 0.0);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(2.0, -2.5, 0.1);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(-2.0, -2.5, 0.1);

      // bottom bench right
      glNormal3f(0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex3f(2.0, -3.5, 0);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(2.0, -2.5, 0);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(2.0, -2.5, 0.1);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(2.0, -3.5, 0.1);

      // bottom bench left
      glNormal3f(0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex3f(-2.0, -3.5, 0);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(-2.0, -2.5, 0);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(-2.0, -2.5, 0.1);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(-2.0, -3.5, 0.1);

      // Top bench front
      glNormal3f(0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex3f(-2.0, 3.5, 0.0);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(2.0, 3.5, 0.0);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(2.0, 3.5, 0.1);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(-2.0, 3.5, 0.1);

      // Top back
      glNormal3f(0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex3f(-2.0, 2.5, 0.0);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(2.0, 2.5, 0.0);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(2.0, 2.5, 0.1);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(-2.0, 2.5, 0.1);

      // Top right
      glNormal3f(0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex3f(2.0, 3.5, 0);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(2.0, 2.5, 0);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(2.0, 2.5, 0.1);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(2.0, 3.5, 0.1);

      // Top left
      glNormal3f(0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex3f(-2.0, 3.5, 0);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(-2.0, 2.5, 0);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(-2.0, 2.5, 0.1);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(-2.0, 3.5, 0.1);
    glEnd;

  glPopName;
  glPopMatrix;

  last_index := 1;

  if prohibit_render then exit;

  RenderHandArr(p_hand, -3.5, -3.75 - 0.8, -0.1);

  RenderBench(p_benched_cards, -1.9, -3.4);

  if p_active > -1 then
  begin
    glPushMatrix;
    inc(last_index);
    glPushName(last_index);
    glTranslatef(0.0, 1.5, -10.0);
    glRotate(FPanAngleX, 0, 1, 0);
    glRotate(FPanAngleY, 1, 0, 0);
    glRotate(315, 1, 0, 0);
    glTranslatef(-0.7125, -2.25, 0);
    if last_index = last_pick then
    begin
      glTranslatef(0, 0, 0.2);
      glRotate(20, 1, 0, 0);
    end;
    FRenderCache[p_active].Draw(modelActive);
    FRenderCache[p_active].name := last_index;
    glPopName;
    glPopMatrix;
  end;

  // Static primitives (no names)
  RenderDeck(p_deck_length, 2.5, -1);
  RenderDeck(o_deck_length, -2.5, 1, 180);

  RenderPrizes(p_prize_cards, -3, -1.75);
  RenderPrizes(o_prize_cards, 2.5, 0.95);

  RenderDiscard(p_discard, 2.5, -2);
  RenderDiscard(o_discard, -2.5, 2, 180);

  RenderOpBench(o_benched_cards, -1.9, -3.4);

  RenderOpHandArr(o_hand, 3.5, 3.75, -0.1);

  if o_active > -1 then
  begin
    glPushMatrix;
    glTranslatef(0.0, 1.5, -10.0);
    glRotate(FPanAngleX, 0, 1, 0);
    glRotate(FPanAngleY, 1, 0, 0);
    glRotate(315, 1, 0, 0);
    glRotate(180, 0, 0, 1);
    glTranslatef(-0.7125, -2.25, 0);
    FRenderCache[o_active].Draw(modelActive);
    glPopMatrix;
  end;

  glPushMatrix;
    glTranslatef(0.0, 1.5, -10.0);
    glRotate(FPanAngleX, 0, 1, 0);
    glRotate(FPanAngleY, 1, 0, 0);
    glRotate(315, 1, 0, 0);
    glTranslate(0, -4.8, 0);
    glBindTexture(GL_TEXTURE_2D, FFontTexture);
    if Turn then
      glImgWrite('Your turn; ' + stage + ' stage')
    else
      glImgWrite('Oponent'#39's turn; ' + stage + ' stage');
  glPopMatrix;

  glPushMatrix;
    glTranslatef(0.0, 1.5, -10.0);
    glRotate(FPanAngleX, 0, 1, 0);
    glRotate(FPanAngleY, 1, 0, 0);
    glRotate(315, 1, 0, 0);
    glTranslate(0, -5, 0);
    glBindTexture(GL_TEXTURE_2D, FFontTexture);
    glImgWrite('Previous render: ' + last_render_ticks.ToString + 'ms');
  glPopMatrix;

  glPushMatrix;
    glTranslatef(0.0, 1.5, -10.0);
    glRotate(FPanAngleX, 0, 1, 0);
    glRotate(FPanAngleY, 1, 0, 0);
    glRotate(315, 1, 0, 0);
    glTranslate(0, -5.2, 0);
    glBindTexture(GL_TEXTURE_2D, FFontTexture);
    glImgWrite('Render Cache Size: ' + length(FRenderCache).ToString);
  glPopMatrix;

  glPushMatrix;
    glTranslatef(0.0, 1.5, -10.0);
    glRotate(FPanAngleX, 0, 1, 0);
    glRotate(FPanAngleY, 1, 0, 0);
    glRotate(315, 1, 0, 0);
    glTranslate(0, -5.4, 0);
    glBindTexture(GL_TEXTURE_2D, FFontTexture);
    glImgWrite('Last Picked: ' + last_pick.ToString);
  glPopMatrix;
end;

procedure TGameUI.HandleCardClick(Context: TPickContext);
begin
  if (turn) and (stage <> 'awaiting') then
    case UpCase(Context.FoundIn[1]) of
      'A':
        begin
          Self.Visible := false;
          ActiveUI.Visible := true;
          ActiveUI.Benched := p_benched_cards;
          ActiveUI.ShowWithState(context.Model, stage, procedure (CallContext: TModelViewerContext)
            begin
              self.Visible := true;
              activeUI.Visible := false;
              self.SetFocus;
              if callcontext <> nil then
              begin
                if Credentials <> nil then
                begin
                  if callcontext.action <> nil then
                  begin
                    // Authenticate the payload
                    callcontext.action.AddPair(Credentials);
                    // send it!
                    IOHandler.Writeln(callcontext.action.ToString);
                    // prevent further actions
                    stage := 'awaiting';
                  end;
                end;
              end;
            end);
        end;
      'H':
        begin
          Self.Visible := false;
          HandUI.Visible := true;
          HandUI.Benched := p_benched_cards;
          HandUI.RenderCache := FRenderCache;
          HandUI.ShowWithFullState(context.Model, stage, FLastState, context.index, procedure (CallContext: TModelViewerContext)
            begin
              self.Visible := true;
              HandUI.Visible := false;
              self.SetFocus;
              if callcontext <> nil then
              begin
                if Credentials <> nil then
                begin
                  if callcontext.action <> nil then
                  begin
                    // Authenticate the payload
                    callcontext.action.AddPair(Credentials);
                    // send it!
                    IOHandler.Writeln(callcontext.action.ToString);
                    // prevent further actions
                    stage := 'awaiting';
                  end;
                end;
              end;
            end);
        end;
      'B':
        begin
          Self.Visible := false;
          BenchUI.Visible := true;
          BenchUI.index := context.Index;
          BenchUI.ShowWithState(context.Model, stage, procedure (CallContext: TModelViewerContext)
            begin
              self.Visible := true;
              activeUI.Visible := false;
              self.SetFocus;
              if callcontext <> nil then
              begin
                if Credentials <> nil then
                begin
                  if callcontext.action <> nil then
                  begin
                    // Authenticate the payload
                    callcontext.action.AddPair(Credentials);
                    // send it!
                    IOHandler.Writeln(callcontext.action.ToString);
                    // prevent further actions
                    stage := 'awaiting';
                  end;
                end;
              end;
            end);
        end;
    end;
end;

procedure TGameUI.HandleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_F1:
      TAnonymousThread.Create(procedure
      var
        i : integer;
      begin
        for I := Low(FRenderCache) to High(FRenderCache) do
        begin
          freeandnil(FRenderCache[i]);
        end;
        setlength(FRenderCache, 0);
        renderstate(flaststate);
      end).start;

    VK_F2:
      RenderState(FLastState);

    VK_F3:
      begin
        RELOADPKMCARDVAR;
        Showmessage('Reloaded card generation varables');
      end;
  end;
end;

procedure TGameUI.HandleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Pick(x, y);
  if last_pick > 1 then
  begin
    if last_pick < length(p_hand) + 2 then
    begin
      HandleCardClick(TPickContext.Create(last_pick - 2,
        FRenderCache[p_hand[last_pick - 2]],
        'hand'));
    end
    else if last_pick < length(p_hand) + 2 + length(p_benched_cards) then
    begin
      HandleCardClick(TPickContext.Create(last_pick - 2 - Length(p_hand),
        FRenderCache[p_benched_cards[last_pick - 2 - Length(p_hand)]],
        'bench'
        ));
    end
    else if last_pick = length(p_hand) + 2 + length(p_benched_cards) then
    begin
      HandleCardClick(TPickContext.Create(0,
        FRenderCache[p_active],
        'active'
        ));
    end;

  end;
end;

procedure TGameUI.HandleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  FPanAngleX := (8 * (x / ClientWidth)) - 4;
  // FPanAngleX := (180 * (x / ClientWidth)) - 90; // View Whole model pan - for debugging
  FPanAngleY := (4.5 * (y / ClientHeight)) - 2.25;
  Pick(X, Y);
  Draw;
  SwapBuffers(wglGetCurrentDC);
end;

procedure TGameUI.HandleResize(Sender: TObject);
begin
  // Change the Matrix to accommodate for the new aspect ratio
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(45.0, ClientWidth / ClientHeight, 2.0, 25.0);
  // position viewer
  glMatrixMode(GL_MODELVIEW);

  glViewport(0, 0, ClientWidth, ClientHeight);

  // Force a render cycle
  Invalidate;
end;

procedure TGameUI.IncomingChat(s: string);
begin
  //ChatMemo.Lines.Add(s);
  //showmessage(s);
end;

procedure TGameUI.OpenChat(sender: Tobject);
begin
  //showmessage('ok');
  //renderState(FLastState);
  //OnChat(Inputbox('Chat', 'Enter message', ''));
end;

procedure TGameUI.Paint;
begin
  inherited;
  Draw;
  SwapBuffers(wglGetCurrentDC);
end;

procedure TGameUI.Pick(X, Y: Integer);
var
  buf: array [0 .. 400] of GLuint;
  viewport: array [0 .. 3] of Integer;
  projection: array [0 .. 3] of GLdouble;
  hits: Integer;
  ptr, ptrNames: ^GLint;
  names, numberOfNames: GLint;
  i: Integer;
  minZ: Integer;
begin
  last_pick := 0;

  glSelectBuffer(100, addr(buf)); // Create the select buffer
  glRenderMode(GL_SELECT); // For debugging comment out this line
  glInitNames; // Initialize / clear the names buffer
  glMatrixMode(GL_PROJECTION); // Configure the matrix
  glPushMatrix;  // Save the current matrix to restore later
  glGetIntegerv(GL_VIEWPORT, @viewport); // Get the value of the viewport into a buffer
  glLoadIdentity;
  // Basically what this code does is it zooms into one pixel area on the screen
  gluPickMatrix(X, viewport[3] - Y, 1, 1, addr(viewport));
  gluPerspective(45.0, ClientWidth / ClientHeight, 2.0, 25.0);
  // glFrustum(-1.6, 1.6, -0.9, 0.9, 2.0, 25.0);
  glMatrixMode(GL_MODELVIEW);

  // Render the scene to the buffer
  glPushMatrix;
  Draw;
  // SwapBuffers(wglGetCurrentDC); // For Debugging Uncomment this line
  glPopMatrix;

  hits := glRenderMode(GL_RENDER);
  // Process the hits (Objects that have been rendered)
  if hits > 0 then
  begin
    ptr := @buf;
    minZ := $FFFFFFFF;

    for i := 0 to hits - 1 do
    begin
      names := ptr^;
      inc(ptr);
      if ptr^ < minZ then
      begin
        numberOfNames := names;
        minZ := ptr^;
        ptrNames := ptr;
        inc(ptrNames, 2);
      end;
      inc(ptr, names + 2);
    end;

    ptr := ptrNames;

    for i := 0 to numberOfNames - 2 do
    begin
      inc(ptr);
    end;
    last_pick := ptr^;
  end;
  // end hit processing

  // Return to the original matrix
  glMatrixMode(GL_PROJECTION);
  glPopMatrix;

  glMatrixMode(GL_MODELVIEW);
end;

// Function used below
procedure TGameUI.PopulateArray (var _fill: TArray<integer>; prop: string; plr_state: TJSONObject);
var
  arr : TJSONArray;
  fill :  TArray<integer>;
  I: Integer;
begin
  // SetLength(fill, length(_fill));

  if plr_state.Exists(prop) then
  begin
    // Assume it is array. Might require validation in future but works for now.
    arr := TJSONArray(plr_state.Get(prop).JsonValue);
    setlength(fill, arr.count);
    
    for I := 0 to arr.Count - 1 do
    begin
      LocateInCache(TJSONObject(arr.Get(i)).ToString, procedure (found: boolean; index: integer)
      begin
        if found then
        begin
          fill[i] := index;
        end
        else
        begin
          setlength(FRenderCache, length(FRenderCache) + 1);
          FRenderCache[high(FRenderCache)] := TCardModel.Create;
          try
            FRenderCache[high(FRenderCache)].Sprite.Data := TJSONObject(arr.Get(i));
          except
            showmessage('whoops, render error');
          end;
          fill[i] := high(FRenderCache);
        end;
      end);
    end;
  end;

  _fill := fill;
end;

// Pass the data JSONObject as state argument;
// This function should always be run in a anonymous or seperate thread to the main
// opengl thread to prevent blocking
procedure TGameUI.RenderState(state: TJSONObject);
var
  ticks : integer;
  json: TJSONObject;
  // This was usefull trust me!
  procedure ClearArray (var arr: TArray<integer>);
  begin
    setlength(arr, 0);
  end;
begin
  FLastState := state;
  ticks := TThread.GetTickCount;

  prohibit_render := true;
  // Clear the variables for new data to be loaded
  // Keep memory clean

  ClearArray(p_benched_cards);
  ClearArray(p_hand);
  ClearArray(p_discard);
  ClearArray(o_benched_cards);
  ClearArray(o_discard);

  p_deck_length := 0;
  p_prize_cards := 0;
  p_active := -1;
  o_deck_length := 0;
  o_prize_cards := 0;
  o_hand := 0;
  o_active := -1;

  // Build the new state
  // build player state ( p_ )
  state.ExistCall('player', procedure (plr_state: TJSONPair)
  begin
    TJSONObject(plr_state.JsonValue).ExistCall('deck', procedure (s: TJSONPair)
    begin
      LocateInCache(TJSONObject(s.JsonValue).ToString, procedure (found: boolean; index: integer)
      begin
        if found then p_deck := index
        else 
        begin
          setlength(FRenderCache, length(FRenderCache) + 1);
          FRenderCache[high(FRenderCache)] := TCardModel.Create;
          try
            FRenderCache[high(FRenderCache)].Sprite.Data := TJSONObject(s.JsonValue);
          except
            showmessage('whoops, render error');
          end;
          p_deck := high(FRenderCache);
        end;
      end);
    end);

    TJSONObject(plr_state.JsonValue).ExistCall('deck-length', procedure (s: TJSONPair)
    begin
      try
        p_deck_length := strtoint(s.JsonValue.Value);
      except
        // fallback incase strtoint fails
        p_deck_length := 0;
      end;
    end);

    TJSONObject(plr_state.JsonValue).ExistCall('prize-cards', procedure (s: TJSONPair)
    begin
      try
        p_prize_cards := strtoint(s.JsonValue.Value);
      except
        p_prize_cards := 0;
      end;
    end);

    PopulateArray(p_hand, 'hand', TJSONObject(plr_state.JsonValue));

    PopulateArray(p_benched_cards, 'benched-cards', TJSONObject(plr_state.JsonValue));

    PopulateArray(p_discard, 'discard', TJSONObject(plr_state.JsonValue));

    TJSONObject(plr_state.JsonValue).ExistCall('active-card', procedure (s: TJSONPair)
    begin
      LocateInCache(TJSONObject(s.JsonValue).ToString, procedure (found: boolean; index: integer)
      begin
        if found then
        begin
          p_active := index
        end
        else
        begin
          setlength(FRenderCache, length(FRenderCache)+1);
          FRenderCache[high(FRenderCache)] := TCardModel.Create;
          try
            FRenderCache[high(FRenderCache)].Sprite.Data := TJSONObject(s.JsonValue);
          except
            showmessage('whoops, render error');
          end;
          p_active := high(FRenderCache);
        end;
      end);
    end);
  end);

  state.ExistCall('oponent', procedure (plr_state: TJSONPair)
  begin
    TJSONObject(plr_state.JsonValue).ExistCall('deck-length', procedure (s: TJSONPair)
    begin
      try
        o_deck_length := strtoint(s.JsonValue.Value);
      except
        // fallback incase strtoint fails
        o_deck_length := 0;
      end;
    end);

    TJSONObject(plr_state.JsonValue).ExistCall('prize-cards', procedure (s: TJSONPair)
    begin
      try
        o_prize_cards := strtoint(s.JsonValue.Value);
      except
        o_prize_cards := 0;
      end;
    end);

    PopulateArray(o_benched_cards, 'benched-cards', TJSONObject(plr_state.JsonValue));

    TJSONObject(plr_state.JsonValue).ExistCall('hand', procedure (s: TJSONPair)
    begin
      try
        o_hand := StrToInt(s.JsonValue.Value);
      except
        o_hand := 0;
      end;
    end);

    PopulateArray(o_discard, 'discard', TJSONObject(plr_state.JsonValue));

    TJSONObject(plr_state.JsonValue).ExistCall('active-card', procedure (s: TJSONPair)
    begin
      LocateInCache(TJSONObject(s.JsonValue).ToString, procedure (found: boolean; index: integer)
      begin
        if found then o_active := index
        else 
        begin
          setlength(FRenderCache, length(FRenderCache)+1);
          FRenderCache[high(FRenderCache)] := TCardModel.Create;
          try
            FRenderCache[high(FRenderCache)].Sprite.Data := TJSONObject(s.JsonValue);
          except
            showmessage('whoops, render error');
          end;
          o_active := high(FRenderCache);
        end;
      end);
    end);
  end);

  turn := False;
  if state.Exists('gameplay') then
  begin
    json := TJSONObject(state.Get('gameplay').JsonValue);

    if json.Exists('turn') then
      if json.Get('turn').JsonValue.Value = 'player' then turn := true;

    if json.Exists('stage') then stage := json.Get('stage').JsonValue.Value;
  end; // end gameplay;

  ticks := TThread.GetTickCount - ticks;
  last_render_ticks := ticks;

  // RENDER THE STATE!!
  TThread.Synchronize(nil, procedure
  begin
    prohibit_render := false;
    Update;
  end);
end;

procedure TGameUI.SetCredentials(const Value: TJSONPair);
begin
  FCredentials := Value;
end;

procedure TGameUI.SetIOhandler(const Value: TIdIOHandler);
begin
  FIOhandler := Value;
end;

procedure TGameUI.SetOnChat(const Value: TChatEvent);
begin
  FOnChat := Value;
end;

procedure TGameUI.Update;
begin
  // Update
  Draw;
  SwapBuffers(wglGetCurrentDC);
end;

{ TPickContext }

constructor TPickContext.Create(_index: integer; _model: TCardModel;
  found_in: string);
begin
  inherited Create;
  Index := _index;
  Model := _model;
  FoundIn := found_in;
end;

end.
