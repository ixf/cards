

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

  constructor (id, cat, frontimg, backimg, Size, side) {
    this.id = id;
    this.cat = cat;
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
    this.group.position.set(pos.x, pos.y, parseInt(this.id));
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

  remove(){
    scene[this.group].remove();
  }

}

class Blocker { 

  constructor(Size, owner){
    this.size = Size;
    this.owner = owner;
    let color = cursors[owner].color;

    this.opacity = (owner == player_name ? 0.2 : 1.0);

    var geometry = new THREE.PlaneGeometry( Size.w, Size.h, 1 );
    var material = new THREE.MeshBasicMaterial( {color: color, opacity: this.opacity, transparent: true, side: THREE.DoubleSide} );
    this.plane = new THREE.Mesh( geometry, material );
    this.plane.position.set(Size.x,Size.y,600);
    this.plane.owner_player = owner;
    scene.add( this.plane );
  }

  remove(){
    scene.remove(this.plane);
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
    context.lineWidth = 10;
    context.strokeText( text, 0, fontsize );
    context.fillText( text, 0, fontsize );

    var texture = new THREE.Texture(canvas) 
    texture.needsUpdate = true;

    var spriteMaterial = new THREE.SpriteMaterial( { map: texture } );
    this.sprite = new THREE.Sprite( spriteMaterial );

    this.sprite.scale.set(128,72,1.0);
  }
}

class Cursor {

  constructor(name, x, y, color){
    this.name = name;
    this.color = color;

    this.blockers = [];

    var map = new THREE.TextureLoader().load( "static/images/cursor.png" );
    var material = new THREE.SpriteMaterial( { map: map, color: color } );
    this.sprite = new THREE.Sprite( material );
    this.sprite.position.set(-40, 0, 800);
    this.sprite.scale.set(40, 40, 1);

    this.sprite.thisis = "cursor";

    this.text = new Text(name);
    this.text.sprite.position.set(0,0, 800);
  }

  setPos(mouse){
    this.pos = {x: mouse.x, y: mouse.y };
    this.sprite.position.set(mouse.x, mouse.y, 800);
    this.text.sprite.position.set(mouse.x+80,mouse.y-40, 800);
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
var total_offset = {x: 0, y:0};

function offset_camera(x, y){
  total_offset.x += x;
  total_offset.y += y;
  camera.position.x = total_offset.x;
  camera.position.y = total_offset.y;
}

var renderer = new THREE.WebGLRenderer();
renderer.setSize( width, height );
var canvas = renderer.domElement;
document.body.appendChild( renderer.domElement );
camera.position.z = 900;




// ### player setup

var raycaster = new THREE.Raycaster();
var mouse = new THREE.Vector2();
var rel_mouse = new THREE.Vector2();

function loadCard(key, cardInfo){
  let card = new Card(key, cardInfo.cat, cardInfo.img, cardInfo.back, cardInfo.size, cardInfo.side);
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

  rel_mouse.x = x - width / 2 + 20 + total_offset.x;
  rel_mouse.y = - y + height / 2 - 20 + total_offset.y;

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

    for(var key in intersects){
      let selected = intersects[key];

      if(selected.object.owner_player != undefined){
	if(selected.object.owner_player != player_name)
	  break;
      } else if("card" in selected.object){
	ws.send(JSON.stringify({action:'cardclick', params: {id: selected.object.card.id}}));
	break;
      }
    }

  } );


document.addEventListener('keydown', function(event) {
  var card = holds[player_name];
  switch(event.key){
    case 'f':
      if(card != undefined){
	if(card.canFlip()){
	  ws.send(JSON.stringify({action:'rotate', params: {'id': card.id}})); 
	}
      }
      break;
    case 'r':
      // wybieranie losowe:
      // tymczasowe, zanim dodane beda stosy i tasowanie
      if(player_name in holds){
	break;
      }

      raycaster.setFromCamera(mouse, camera);
      var intersects = raycaster.intersectObjects( scene.children, true );

      var all_valid = [];
      for(var key in intersects){
	let selected = intersects[key];

	if(selected.object.owner_player != undefined){
	  if(selected.object.owner_player != player_name)
	    break;
	} else if("card" in selected.object){
	  all_valid.push(selected.object.card.id);
	}
      }

      if(all_valid.length > 0){
	let index = Math.floor(Math.random() * all_valid.length);
	let id = all_valid[index];
	let card = cards[id];
	ws.send(JSON.stringify({action:'cardclick', params: {id: id}}));
	ws.send(JSON.stringify({action:'random_choice', params: {name: player_name, cat: card.cat }}));
      }

      break;
    case 'ArrowDown':
      offset_camera(0, -50);
      break;
    case 'ArrowUp':
      offset_camera(0, 50);
      break;
    case 'ArrowLeft':
      offset_camera(-50,0);
      break;
    case 'ArrowRight':
      offset_camera(50,0);
      break;
  }


  canvas.focus();

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

function chatlog(txt){
  document.getElementById("chatlog").innerHTML += "<br>" + txt;
}


var onJoin = {};
ws.addEventListener('message', function (event) {
  var obj = JSON.parse(event.data)
  var action = obj.action;

  switch(action){
    case "carddown":
      var player = obj.params.name;
      var clicked = cards[obj.params.id];
      clicked.click(0);
      delete holds[player];
      break;

    case "cardup":
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

    case "delcard":
      cards[obj.params.id].remove();
      delete cards[obj.params.id];
      break;

    case "blockernew":
      let owner = obj.params.owner;
      let size = obj.params.size;
      if ( owner in cursors ){
	cursors[owner].blockers.push( new Blocker(size, owner) );
      } else {
	if(onJoin[owner] == undefined){
	  onJoin[owner] = [];
	}
	onJoin[owner].push(function() {
	  cursors[owner].blockers.push( new Blocker(size, owner) );
	});
      }
      
      break;

    case "cardmove":
      // todo
      break;

    case "playerjoined":
      loadNewCursor(obj.params);
      chatlog(obj.params.name + " dołączył" );
      if( obj.params.name in onJoin ){
	onJoin[obj.params.name].forEach( (f) => f() );
	delete onJoin[obj.params.name];
      }
      break;

    case "playerleft":
      let c = cursors[obj.params.name];
      chatlog(obj.params.name + " zwiał" );
      extra_scene.remove(c.sprite);
      extra_scene.remove(c.text.sprite);
      c.blockers.forEach( (b) => b.remove() );
      delete cursors[obj.params.name];
      break;

    case "playermoved":
      movePlayer(obj.params.name, obj.params.newpos);
      if( obj.params.name in holds ){
        var card = holds[obj.params.name];
        card.setPos(obj.params.newpos);
      }
      break;

    case "clear_chatlog":
      document.getElementById("chatlog").innerHTML = "witaj w ulica karasiowa the gaem";
      break;

    case "random_choice":
      chatlog(obj.params.name + " losuje! kategoria " + obj.params.cat);
      break;

    case "eyes_closed":
      canvas.style.display = "none";
      break;

    case "eyes_open":
      canvas.style.display = "block";
      break;

    default:
      console.log('weird msg from server ', obj);
  }
});

window.onbeforeunload = function() {
  ws.onclose = function () {};
  ws.close();
};


renderer.setClearColor(0x493a4c);
animate();

function animate() {
  requestAnimationFrame( animate );
  // var timer = Date.now() * 0.0001;
  renderer.render( extra_scene, camera );
}

