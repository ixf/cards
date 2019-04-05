

var player_name = findGetParameter("name") || "player"
var cursor_color = findGetParameter("color") || "white"

// ### logic setup 
class Size { 

  constructor (x, y, w, h){
    this.x = x;
    this.y = y;
    this.w = w;
    this.h = h;
  }
}

class Card {

  constructor (id, frontimg, backimg, Size, side) {
    this.id = id;
    this.Size = Size;

    this.state = 0;
    // 0 == laying down
    // 1 == picked up

    this.side = side;
    this.scale = 0.4;

    let front = new THREE.TextureLoader().load( frontimg );
    let back = new THREE.TextureLoader().load( backimg );
    let frontgeometry = new THREE.PlaneGeometry( 1, 1 );
    let backgeometry = new THREE.PlaneGeometry( 1, 1 );
    backgeometry.applyMatrix( new THREE.Matrix4().makeRotationY( Math.PI ) );

    let frontmat = new THREE.MeshBasicMaterial( { map: front, transparent: true } );
    let backmat = new THREE.MeshBasicMaterial( { map: back, transparent: true } );

    let mesh = new THREE.Mesh( frontgeometry, frontmat );
    let bmesh = new THREE.Mesh( backgeometry, backmat );
    mesh.card = this;
    bmesh.card = this;

    this.group = new THREE.Group();
    this.group.add(mesh);
    this.group.add(bmesh);

    this.group.scale.set(this.Size.w * this.scale, this.Size.h * this.scale, 1);
    if( side == 1 )
      this.group.rotation.y = Math.PI;
  }

  click(state){
    if(state == 1){
      // this.state = 1;
      this.group.scale.set(1.2 * this.Size.w * this.scale, 1.2 * this.Size.h * this.scale, 1);
    } else {
      // this.state = 0;
      this.group.scale.set(this.Size.w * this.scale, this.Size.h * this.scale, 1);
    }
  }

  setPos(pos){
    this.group.position.set(pos.x, pos.y, 0);
  }

  canFlip(){
    return !this.flipping;
  }

  startFlip(){

    this.flipping = true;
    let final = (this.side == 1) ? 0 : Math.PI;
    this.side = (this.side + 1) % 2;

    const frames_total = 20;
    const frames_time = 16;
    const rot_val = Math.PI / frames_total;
    let m = this.group;
    let t = this;

    let interval = setInterval(function(){ 
      m.rotation.y += rot_val;
    }, 16);

    setTimeout(function(){
      t.flipping = false;
      clearInterval(interval);
      m.rotation.y = final;
    }, frames_total * frames_time);
  }

}

class Text{
  constructor( text ){
    var fontface = "Arial";
    var fontsize = 40;

    var canvas = document.createElement('canvas');
    var context = canvas.getContext('2d');
    context.font = "Bold " + fontsize + "px " + fontface;

    var metrics = context.measureText( text );

    context.fillStyle = "white";
    context.strokeStyle = "black";
    context.fillText( text, 0, fontsize );
    context.strokeText( text, 0, fontsize);

    var texture = new THREE.Texture(canvas) 
    texture.needsUpdate = true;

    var spriteMaterial = new THREE.SpriteMaterial( 
      { map: texture } );
    this.sprite = new THREE.Sprite( spriteMaterial );

    this.sprite.scale.set(128,72,1.0);
  }
}

class Cursor {

  constructor(name, x, y, color){
    this.name = name;
		this.color = color;

    var map = new THREE.TextureLoader().load( "static/images/cursor.png" );
    var material = new THREE.SpriteMaterial( { map: map, color: color } );
    this.sprite = new THREE.Sprite( material );
    this.sprite.position.set(-40, 0, 90);
    this.sprite.scale.set(40, 40, 1);

    this.sprite.thisis = "cursor";

    this.text = new Text(name);
    this.text.sprite.position.set(0,0, 90);
  }

  setPos(mouse){
    this.pos = {x: mouse.x, y: mouse.y };
    this.sprite.position.set(mouse.x, mouse.y, 90);
    this.text.sprite.position.set(mouse.x+80,mouse.y-40, 90);
  }
}

function findGetParameter(parameterName) {
  var result = null,
    tmp = [];
  location.search
    .substr(1)
    .split("&")
    .forEach(function (item) {
      tmp = item.split("=");
      if (tmp[0] === parameterName) result = decodeURIComponent(tmp[1]);
    });
  return result;
}

var my_cursor_pos = {};
var cursors = [];
var cards = [];
var holds = []; // name : card id


// ### scene setup

const width = 1600;
const height = 900;

var scene = new THREE.Scene();
var extra_scene = new THREE.Scene();
extra_scene.add(scene);
var camera = new THREE.OrthographicCamera( width / - 2, width / 2, height / 2, height / - 2, 0.1, 1000 );

var renderer = new THREE.WebGLRenderer();
renderer.setSize( width, height );
var canvas = renderer.domElement;
document.body.appendChild( renderer.domElement );
camera.position.z = 100;


// ### player setup

var raycaster = new THREE.Raycaster();
var mouse = new THREE.Vector2();
var rel_mouse = new THREE.Vector2();

function loadCard(key, cardInfo){
  let card = new Card(key, cardInfo.img, cardInfo.back, cardInfo.size, cardInfo.side);
  card.setPos(cardInfo.pos);
  cards[key] = card;
  scene.add(card.group);
}

function loadCards(received){
  for (const [key, cardInfo] of Object.entries(received)){
    loadCard(key,cardInfo);
  }
}

function movePlayer(who, where){
  if (who in cursors)
    cursors[who].setPos(where);
}


function loadNewCursor(newPlayer){
  var new_cursor = new Cursor(newPlayer.name, 0, 0, newPlayer.color);
  cursors[newPlayer.name] = new_cursor;
  extra_scene.add( new_cursor.sprite );
  extra_scene.add( new_cursor.text.sprite );
}

function loadCursors(players){

  for (const [key, value] of Object.entries(players)){
    var new_cursor = new Cursor(key, value.pos.x, value.pos.y, value.color);
    cursors[key] = new_cursor;
    extra_scene.add( new_cursor.sprite );
    extra_scene.add( new_cursor.text.sprite );
  }

}


canvas.addEventListener('mousemove', function(event){
  var rect = canvas.getBoundingClientRect();
  var x = event.clientX - rect.left;
  var y = event.clientY - rect.top;

  rel_mouse.x = x - width / 2 + 20;
  rel_mouse.y = - y + height / 2 - 20;

  mouse.x = ( x / width ) * 2 - 1;
  mouse.y = - ( y / height ) * 2 + 1;

  my_cursor_pos = { x: rel_mouse.x, y: rel_mouse.y };

}, false);

canvas.addEventListener('mousedown', 
  function onMouseDown(event) {

    if(player_name in holds){
      ws.send(JSON.stringify({action:'cardclick', params: {id: holds[player_name].id}}));
      return;
    }

    raycaster.setFromCamera(mouse, camera);
    var intersects = raycaster.intersectObjects( scene.children, true );

    console.log(intersects);
    if(intersects.length > 0){
      var selected = intersects[0];
      console.log(selected);
      ws.send(JSON.stringify({action:'cardclick', params: {id: selected.object.card.id}}));
    }

  } , false);


document.addEventListener('keydown', function(event) {
  console.log("Keydown: " + event.key);
  var card = holds[player_name];
  switch(event.key){
    case 'f':
      if(card != undefined){
	if(card.canFlip()){
	  ws.send(JSON.stringify({action:'rotate', params: {'id': card.id}})); 
	}
      }
      break;
  }

});



// ### websocket setup

var ws = new WebSocket("ws://" + window.location.hostname + ":8080/game/" + player_name + "/" + cursor_color);

ws.onopen = function(event) {
  //ws.send(JSON.stringify({action:'givestate', params: {}}));

  setInterval(function(){
    if(ws.readyState < 2)
      if(my_cursor_pos.x != undefined){
	ws.send(JSON.stringify({action: "pos_change", params: { "cursor": my_cursor_pos }}));
      }
  }, 16);
};

ws.addEventListener('message', function (event) {
  var obj = JSON.parse(event.data)
  var action = obj.action;

  switch(action){
    case "carddown":
      console.log("carddown", obj);
      var player = obj.params.name;
      var clicked = cards[obj.params.id];
      clicked.click(0);
      delete holds[player];
      break;

    case "cardup":
      console.log("cardup", obj);
      var player = obj.params.name;
      var clicked = cards[obj.params.id];
      clicked.click(1);
      holds[player] = clicked;
      break;

    case "rotate":
      cards[obj.params.id].startFlip();
      break;

    case "newcard":
      cards.push( loadCard(obj.params.id, obj.params) )
      break;

    case "state":
      cards = [];
      cursors = [];
      console.log("obj:", obj);
      //loadCards(obj.params.cards);
      loadCursors(obj.params.players);
      break;

    case "playerjoined":
      console.log(obj);
      loadNewCursor(obj.params);
      break;

    case "playerleft":
      console.log(obj);
      let c = cursors[obj.params.name];
      extra_scene.remove(c.sprite);
      extra_scene.remove(c.text.sprite);
      delete cursors[obj.params.name];
      break;

    case "playermoved":
      movePlayer(obj.params.name, obj.params.newpos);
      if( obj.params.name in holds ){
        var card = holds[obj.params.name];
        card.setPos(obj.params.newpos);
      }
      break;

    default:
      console.log('weird msg from server ', obj);
  }
});

window.onbeforeunload = function() {
  ws.onclose = function () {};
  ws.close();
};


renderer.setClearColor(0x9e6a8c);
animate();

function animate() {
  requestAnimationFrame( animate );
  // var timer = Date.now() * 0.0001;
  renderer.render( extra_scene, camera );
}
