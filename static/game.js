

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

  constructor (id, img, Size) {
    this.id = id;
    this.img = img;
    this.Size = Size;

    this.state = 0;
    // 0 == laying down
    // 1 == picked up

    this.rotation = 0;
    // 0 == downside down
    // 1 == upside down
    //
    this.scale = 0.4;

    var map = new THREE.TextureLoader().load( img );
    var material = new THREE.SpriteMaterial( { map: map, color: 0xffffff } );
    this.sprite = new THREE.Sprite( material );
    this.sprite.scale.set(this.Size.w * this.scale, this.Size.h * this.scale, 1);
    this.sprite.card = this;
  }

  click(state){
    if(state == 1){
      // this.state = 1;
      this.sprite.scale.set(1.2 * this.Size.w * this.scale, 1.2 * this.Size.h * this.scale, 1);
    } else {
      // this.state = 0;
      this.sprite.scale.set(this.Size.w * this.scale, this.Size.h * this.scale, 1);
    }
  }

  setPos(pos){
    this.sprite.position.set(pos.x, pos.y, 0);
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
    console.log(metrics);

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
    this.sprite.position.set(-40, 0, 2);
    this.sprite.scale.set(40, 40, 1);

    this.text = new Text(name);
    this.text.sprite.position.set(0,0, 2);
  }

  setPos(mouse){
    this.pos = {x: mouse.x, y: mouse.y };
    this.sprite.position.set(mouse.x, mouse.y, 2);
    this.text.sprite.position.set(mouse.x+80,mouse.y-40, 2);
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

const width = 1280;
const height = 720;

var scene = new THREE.Scene();
var fakecamera = new THREE.OrthographicCamera( -1, 1, 1, -1, 0.1, 1000 );
var camera = new THREE.OrthographicCamera( width / - 2, width / 2, height / 2, height / - 2, 0.1, 1000 );

var renderer = new THREE.WebGLRenderer();
renderer.setSize( width, height );
var canvas = renderer.domElement;
document.body.appendChild( renderer.domElement );
camera.position.z = 3;


// ### player setup

var raycaster = new THREE.Raycaster();
var mouse = new THREE.Vector2();

function loadCards(received){

  for (const [key, cardInfo] of Object.entries(received)){
    var card = new Card(key, cardInfo.img, cardInfo.size);
    card.setPos(cardInfo.pos);
    cards.push(card);
    scene.add(card.sprite);
  }
}

function movePlayer(who, where){
  cursors.find(function(c) {
    return c.name == who;
  }).setPos(where);
}


function loadNewCursor(newPlayer){
  var new_cursor = new Cursor(newPlayer.name, 0, 0, newPlayer.color);
  cursors.push(new_cursor);
  scene.add( new_cursor.sprite );
  scene.add( new_cursor.text.sprite );
}

function loadCursors(players){

  for (const [key, value] of Object.entries(players)){
    var new_cursor = new Cursor(key, value.pos.x, value.pos.y, value.color);
    cursors.push(new_cursor);
    scene.add( new_cursor.sprite );
    scene.add( new_cursor.text.sprite );
  }

}


document.addEventListener('mousemove', function(event){
  var rect = canvas.getBoundingClientRect();
  var x = event.clientX - rect.left;
  var y = event.clientY - rect.top;

  mouse.x = x - width / 2 + 20;
  mouse.y = - y + height / 2 - 20;

  my_cursor_pos = { x: mouse.x, y: mouse.y };

  // if(selected != null){
  // 	selected.object.position.set( mouse.x, mouse.y,0.0);
  // 	console.log(selected.position);
  // }

}, false);

document.addEventListener('mousedown', 
  function onMouseDown(event) {

    console.log(holds);
    if(player_name in holds){
      ws.send(JSON.stringify({action:'cardclick', params: {id: holds[player_name].id}}));
      return;
    }

    raycaster.setFromCamera(mouse, fakecamera);
    var intersects = raycaster.intersectObjects( scene.children );

    console.log(intersects);
    if(intersects.length > 0){
      var selected = intersects[0];
      ws.send(JSON.stringify({action:'cardclick', params: {id: selected.object.card.id}}));
    }

  } , false);


// document.addEventListener('keydown', function(event) {
//   // console.log("Keydown: " + event.key);
// });
// document.addEventListener('keyup', function(event) {
//   // console.log("Keyup: " + event.key);
// });

// ### websocket setup

var ws = new WebSocket("ws://localhost:8080/game/" + player_name + "/" + cursor_color);

ws.onopen = function(event) {
  ws.send(JSON.stringify({action:'givestate', params: {}}));

  setInterval(function(){
		if(ws.readyState < 2)
			if(my_cursor_pos.x != undefined){
				ws.send(JSON.stringify({action: "update", params: { "cursor": my_cursor_pos }}));
			}
	}, 33);
};

ws.addEventListener('message', function (event) {
  var obj = JSON.parse(event.data)
  var action = obj.action;

  switch(action){
    case "carddown":
      console.log("carddown", obj);
      var player = obj.params.name;
      var clicked = cards[obj.params.id-1];
      clicked.click(0);
      delete holds[player];
      break;

    case "cardup":
      console.log("cardup", obj);
      var player = obj.params.name;
      var clicked = cards[obj.params.id-1];
      clicked.click(1);
      holds[player] = clicked;
      break;

    case "state":
      cards = [];
      cursors = [];
      console.log("obj:", obj);
      loadCards(obj.params.cards);
      loadCursors(obj.params.players);
      break;

    case "playerjoined":
      console.log(obj);
      loadNewCursor(obj.params);
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
  renderer.render( scene, camera );
}
