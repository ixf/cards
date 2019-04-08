

var player_name = findGetParameter("name") || "player"
var cursor_color = findGetParameter("color") || "white"


var my_cursor_pos = {};
var cursors = [];
var cards = [];
var holds = []; // name -> card id


// ### scene setup

const width = 1600;
const height = 900;

var scene = new THREE.Scene();
var extra_scene = new THREE.Scene();
extra_scene.add(scene);
var camera = new THREE.OrthographicCamera( width / - 2, width / 2, height / 2, height / - 2, 0.1, 1000 );
camera.total_offset = {x: 0, y:0};

camera.move = (x, y) => {
  camera.total_offset.x += x;
  camera.total_offset.y += y;
  camera.position.x = camera.total_offset.x;
  camera.position.y = camera.total_offset.y;
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

  rel_mouse.x = x - width / 2 + 20 + camera.total_offset.x;
  rel_mouse.y = - y + height / 2 - 20 + camera.total_offset.y;

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
	ws.send(JSON.stringify({action:'random_choice', params: {name: player_name, cat: card.cat }}));
	ws.send(JSON.stringify({action:'cardclick', params: {id: id}}));
      }

      break;
    case 'ArrowDown':
      camera.move(0, -50);
      break;
    case 'ArrowUp':
      camera.move(0, 50);
      break;
    case 'ArrowLeft':
      camera.move(-50,0);
      break;
    case 'ArrowRight':
      camera.move(50,0);
      break;
  }

  canvas.focus();
});



// ### websocket setup

var ws = new WebSocket("ws://" + window.location.hostname + ":8080/game/" + player_name + "/" + cursor_color);

ws.onopen = function(event) {

  setInterval(function(){
    if(ws.readyState < 2)
      if(my_cursor_pos.x != undefined){
	ws.send(JSON.stringify({action: "pos_change", params: { "cursor": my_cursor_pos }}));
      }
  }, 16);
};

function chatlog(txt){
  let b = document.getElementById("chatlog");
  b.innerHTML += "<br>" + txt;
  b.scrollTop = b.scrollHeight;
}


var onJoin = {};
var eyes_mode = false;

var actions = [];
actions["carddown"] = (params) => {
  var player = params.name;
  var clicked = cards[params.id];
  clicked.click(0);
  delete holds[player];
}

actions["cardup"] = (params) => {
  var player = params.name;
  var clicked = cards[params.id];
  if(eyes_mode){
    chatlog(player + " podnosi karte. kategoria " + clicked.cat);
  }
  clicked.click(1);
  holds[player] = clicked;
}

actions["rotate"] = (params) => {
  cards[params.id].startFlip();
}

actions["newcard"] = (params) => {
  cards.push( loadCard(params.id, params) )
}

actions["delcard"] = (params) => {
  cards[params.id].remove();
  delete cards[params.id];
}

actions["blockernew"] = (params) => {
  let owner = params.owner;
  let size = params.size;
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

}

actions["cardmove"] = (params) => {
  // todo
}

actions["new_player"] = (params) => {
  loadNewCursor(params);
  chatlog(params.name + " dołączył" );
  if( params.name in onJoin ){
    onJoin[params.name].forEach( (f) => f() );
    delete onJoin[params.name];
  }
}

actions["playerleft"] = (params) => {
  let c = cursors[params.name];
  chatlog(params.name + " zwiał" );
  extra_scene.remove(c.sprite);
  extra_scene.remove(c.text.sprite);
  // c.blockers.forEach( (b) => b.remove() );
  delete cursors[params.name];
}

actions["playermoved"] = (params) => {
  cursors[params.name].setPos(params.newpos);
  if( params.name in holds ){
    var card = holds[params.name];
    card.setPos(params.newpos);
  }
}

actions["clear_chatlog"] = (params) => {
  document.getElementById("chatlog").style.display = "none";
  document.getElementById("chatlog").innerHTML = "witaj w ulica karasiowa the gaem";
}

actions["random_choice"] = (params) => {
  chatlog(params.name + " losuje! kategoria " + params.cat);
}

actions["eyes_closed"] = (params) => {
  eyes_mode = true;
  canvas.style.display = "none";
}

actions["eyes_open"] = (params) => {
  eyes_mode = false;
  canvas.style.display = "block";
}

actions["chat"] = (params) => {
  chatlog(params.name + "> " + params.text);
}

ws.addEventListener('message', function (event) {
  var obj = JSON.parse(event.data)
  var action = obj.action;
  if( action in actions ){
    actions[action](obj.params);
  } else {
    console.log('weird msg from server ', obj);
  }
});


window.onload = function () {
  var chatbox = document.getElementById("chatbox");
  var chathistory = document.getElementById("chatlog");
  chatbox.addEventListener("keyup", function(event) {
    if (event.keyCode === 13) {
      event.preventDefault();
      chathistory.style.display = 'block';
      ws.send(JSON.stringify({action:'chat', params: { name: player_name, text: chatbox.value }}));
      chatbox.value = "";
    }
  });
};


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

