/*********
 * made by Matthias Hurrle (@atzedent)
 */

/** @type {HTMLCanvasElement} */
const canvas = window.canvas
const gl = canvas.getContext("webgl2")
const dpr = Math.max(1, window.devicePixelRatio)
/** @type {Map<string,PointerEvent>} */
const touches = new Map()

const vertexSource = `#version 300 es
#ifdef GL_FRAGMENT_PRECISION_HIGH
precision highp float;
#else
precision mediump float;
#endif

in vec2 position;

void main(void) {
    gl_Position = vec4(position, 0., 1.);
}
`
const fragmentSource = `#version 300 es
/*********
* made by Matthias Hurrle (@atzedent)
*/

#ifdef GL_FRAGMENT_PRECISION_HIGH
precision highp float;
#else
precision mediump float;
#endif

out vec4 fragColor;

uniform vec2 resolution;
uniform float time;
uniform int pointerCount;
uniform vec2 touch;

#define T time
#define S smoothstep
#define mouse (touch/resolution)
#define rot(a) mat2(cos(a),-sin(a),sin(a),cos(a))

float rnd(vec2 p) {
  return fract(sin(dot(p.xy, vec2(12.9898, 78.233))) * 43758.5453123);
}

float noise(vec2 p) {
  vec2 f = fract(p),
  i = floor(p);
  float
  a = rnd(i),
  b = rnd(i+vec2(1, 0)),
  c = rnd(i+vec2(0, 1)),
  d = rnd(i+vec2(1, 1));

  vec2 u = f*f*(3.-2.*f);

  return mix(a, b, u.x)+
  (c-a)*u.y*(1.-u.x)+
  (d-b)*u.y*u.x;
}

float box(vec3 p, vec3 s, float r) {
  p = abs(p) - s;

  return length(max(p, .0)) +
    min(.0, max(max(p.x, p.y), p.z)) - r;
}

float egg(vec3 p, vec3 s) {
  return (length(p / s) - 1.) * min(min(s.x, s.y), s.z);
}

float mat = .0;
float map(vec3 p) {
  float d = 5e5,
  flr = box(p + vec3(vec2(0, 1.0125) + sin(p.xz * 2.+1.5*noise(p.xz*.82)) * .08, 0), vec3(9, .05, 9), .005),
  sph = egg(p - vec3(0, .1, 0), vec3(1, 1.3, 1));

  d = min(d, flr);
  d = min(d, sph);

  if(d == sph)
    mat = 1.;
  else
    mat = .0;

  return d;
}

vec3 norm(vec3 p) {
  vec2 e = vec2(1e-3, 0);
  float d = map(p);
  vec3 n = d - vec3(map(p - e.xyy), map(p - e.yxy), map(p - e.yyx));

  return normalize(n);
}

float getshadow(vec3 ro, vec3 rd) {
  const float steps = 10., k = 64.;
  float shade = 1.;

  for(float i = 1e-3; i < steps;) {
    float d = map(ro + rd * i);

    if(d < 1e-3) {
      shade = 5e-3;
      break;
    }

    shade = min(shade, k * d / i);

    i += d;
  }

  return shade;
}

float getsss(vec3 p, vec3 rd, float dist, float k) {
  float ddist = dist * k;
  return clamp(map(p + rd * dist) / dist, .0, 1.) +
    clamp(map(p + rd * ddist) / ddist, .0, 1.);
}

float getao(vec3 p, vec3 n, float dist) {
  return clamp(map(p + n * dist) / dist, .0, 1.);
}

void cam(inout vec3 p) {
  if(pointerCount > 0) {
    p.yz*=rot(-clamp(.0,.55,mouse.y)*acos(-1.)+acos(.0));
    p.xz *= rot((acos(.0) - mouse.x) * acos(-1.) * 2.);
  } else {
    p.xz *= rot(.45);
  }
}

void main(void) {
  vec3 col = vec3(0);
  const float n=1.;
  float mn = min(resolution.x, resolution.y);
  for (float dx=.0; dx<n; dx++) {
  for (float dy=.0; dy<n; dy++) {
  vec2 coord=gl_FragCoord.xy+vec2(dx,dy)*.5,
  uv = (
    gl_FragCoord.xy-.5*resolution
  ) / mn;

  vec3 ro = vec3(0, 0, -4),
  rd = normalize(vec3(uv, 1)),
  lp = vec3(1, 2, -3);

  cam(ro);
  cam(rd);
  cam(lp);

  vec3 p = ro;

  const float steps = 400., maxd = 40.;
  float i = .0, dd = .0, at = .0;

  for(; i < steps; i++) {
    float d = map(p) * .5;

    if(d < 1e-3) {
      if(mat == 1.) {
        col += .7 * vec3(3, 2, 1) * S(.0, .1, sin(20. * p.z));
      } else {
        col += .06 * rnd(p.xz * 5.);
      }
      col += .06 * rnd(rd.yz * 5.);
      break;
    }

    if(dd > maxd) {
      dd = maxd;
      break;
    }

    p += rd * d;
    dd += d;
    at += .1 * (.1 / dd);
  }

  vec3 n = norm(p),
  l = normalize(lp - p);

  float diff = max(.0, dot(l, n)),
  spec = max(.0, dot(n, normalize(lp - rd))),
  fres = 1. - max(.0, dot(-l, n)),
  fog = S(1., .0, dd / maxd),
  glow = S(.0, 1., i / steps),
  ao = getao(p, l, .5) * .5,
  sss = getsss(l, n, 10., 5.),
  shd = getshadow(p + n * 5e-2, l);

  col += pow(fres, 4.) * diff * vec3(7, 4, 2) / 7.;
  col += fres * pow(spec, 128.);

  col *= fog;
  col *= shd * exp(shd * diff) * fres;
  col = mix(col, vec3(diff), diff * ao);
  col = mix(col, vec3(sss * .3), glow);

  col = exp(-col * 8.);
  col = sqrt(col);
  col = exp(-col * 4.);

  col += at * 2e-1;

  col += uv.x * uv.y - .35;
  col *= S(1., .0, dot(uv, uv));
  }}
  
  fragColor = vec4(col, 1);
}
`
let time
let buffer
let program
let touch
let resolution
let pointerCount
let vertices = []
let touching = false

function resize() {
    const { innerWidth: width, innerHeight: height } = window

    canvas.width = width * dpr
    canvas.height = height * dpr

    gl.viewport(0, 0, width * dpr, height * dpr)
}

function compile(shader, source) {
    gl.shaderSource(shader, source)
    gl.compileShader(shader)

    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        console.error(gl.getShaderInfoLog(shader))
    }
}

function setup() {
    const vs = gl.createShader(gl.VERTEX_SHADER)
    const fs = gl.createShader(gl.FRAGMENT_SHADER)

    program = gl.createProgram()

    compile(vs, vertexSource)
    compile(fs, fragmentSource)

    gl.attachShader(program, vs)
    gl.attachShader(program, fs)
    gl.linkProgram(program)

    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        console.error(gl.getProgramInfoLog(program))
    }

    vertices = [-1.0, -1.0, 1.0, -1.0, -1.0, 1.0, -1.0, 1.0, 1.0, -1.0, 1.0, 1.0]

    buffer = gl.createBuffer()

    gl.bindBuffer(gl.ARRAY_BUFFER, buffer)
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW)

    const position = gl.getAttribLocation(program, "position")

    gl.enableVertexAttribArray(position)
    gl.vertexAttribPointer(position, 2, gl.FLOAT, false, 0, 0)

    time = gl.getUniformLocation(program, "time")
    touch = gl.getUniformLocation(program, "touch")
    pointerCount = gl.getUniformLocation(program, "pointerCount")
    resolution = gl.getUniformLocation(program, "resolution")
}

function draw(now) {
    gl.clearColor(0, 0, 0, 1)
    gl.clear(gl.COLOR_BUFFER_BIT)

    gl.useProgram(program)
    gl.bindBuffer(gl.ARRAY_BUFFER, null)
    gl.bindBuffer(gl.ARRAY_BUFFER, buffer)

    gl.uniform1f(time, now * 0.001)
    gl.uniform2f(touch, ...getTouches())
    gl.uniform1i(pointerCount, touches.size)
    gl.uniform2f(resolution, canvas.width, canvas.height)
    gl.drawArrays(gl.TRIANGLES, 0, vertices.length * 0.5)
}

function getTouches() {
    if (!touches.size) {
        return [0, 0]
    }

    for (let [id, t] of touches) {
        const result = [dpr * t.clientX, dpr * (innerHeight - t.clientY)]

        return result
    }
}

function loop(now) {
    draw(now)
    requestAnimationFrame(loop)
}

function init() {
    setup()
    resize()
    loop(0)
}

document.body.onload = init
window.onresize = resize
canvas.onpointerdown = e => {
    touching = true
    touches.set(e.pointerId, e)
}
canvas.onpointermove = e => {
    if (!touching) return
    touches.set(e.pointerId, e)
}
canvas.onpointerup = e => {
    touching = false
    touches.clear()
}
canvas.onpointerout = e => {
    touching = false
    touches.clear()
}