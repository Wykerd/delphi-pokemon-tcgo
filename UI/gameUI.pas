unit gameUI;

interface
// Page 148
uses
  Windows, Classes, Forms, Dialogs, StdCtrls, Graphics, SysUtils, helpers, DBXJSON,
  Controls, ExtCtrls, UIContainer, OpenGL, Textures, pkmCard;

type
  TChatEvent = procedure (s : string) of object;

  TGameUI = class (TUIContainer)
  private
    FState: TJSONObject;
    FOnChat: TChatEvent;
    FBenchedCards : array of TCardModel;
    FPanAngleX, FPanAngleY: Extended;
    FBoardTex, FEdgeTex, FBenchEdgeTex, FBenchTex : GLuint;
    procedure SetOnChat(const Value: TChatEvent);
    procedure SetState(const Value: TJSONObject);
    procedure OpenChat(sender: Tobject);
    procedure HandleResize(Sender: TObject);
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  published
    constructor Create (AOwner: TComponent); override;
    property State : TJSONObject read FState write SetState;
    // Events //
    property OnChat : TChatEvent read FOnChat write SetOnChat;
    // -- //
    // OpenGL //
    procedure Pick (X, Y: Integer);
    procedure Paint; override;
    procedure Draw;
    procedure Init;
    // -- //
    // Handlers //
    procedure IncomingChat (s : string);
    // -- //
  public
  end;

var
  debugTex : GLuint;

implementation

{ GL Setup Functions }
procedure setupPixelFormat(DC: HDC);
const
  pfd: TPIXELFORMATDESCRIPTOR = (nSize: sizeof(TPIXELFORMATDESCRIPTOR); // size
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
  // Default textures to enabled
  glEnable(GL_TEXTURE_2D);
end;

{ TGameUI }

constructor TGameUI.Create(AOwner: TComponent);
begin
  inherited;
  OnClick := OpenChat;
  OnMouseMove := HandleMouseMove;
  OnResize := HandleResize;
  SetLength(FBenchedCards, 0);
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
end;

procedure TGameUI.Draw;
var
  I: Integer;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

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

  for I := 2 to Length(FBenchedCards) + 1 do
  begin
    glPushMatrix;
    glPushName(i);
    glTranslatef(0.0, 1.5, -10.0);
    glRotate(FPanAngleX, 0, 1, 0);
    glRotate(FPanAngleY, 1, 0, 0);
    glRotate(315, 1, 0, 0);
      FBenchedCards[i - 2].Draw;
    glPopName;
    glPopMatrix;
  end;
end;

procedure TGameUI.HandleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  FPanAngleX := (8 * (x / ClientWidth)) - 4;
  //FPanAngleX := (180 * (x / ClientWidth)) - 90; // View Whole model pan
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
end;

procedure TGameUI.OpenChat(sender: Tobject);
begin
  OnChat(Inputbox('Chat', 'Enter message', ''));
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

    for i := 0 to numberOfNames - 1 do
    begin
      // showmessage(IntToStr(ptr^));
      inc(ptr);
    end;
  end;
  // end hit processing

  // Return to the original matrix
  glMatrixMode(GL_PROJECTION);
  glPopMatrix;

  glMatrixMode(GL_MODELVIEW);
end;

procedure TGameUI.SetOnChat(const Value: TChatEvent);
begin
  FOnChat := Value;
end;

procedure TGameUI.SetState(const Value: TJSONObject);
begin
  FState := Value;
end;

end.
