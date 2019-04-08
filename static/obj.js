
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

    let geometry = new THREE.PlaneGeometry( Size.w, Size.h, 1 );
    let material = new THREE.MeshBasicMaterial( {color: color, opacity: this.opacity, transparent: true, side: THREE.DoubleSide} );
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
    let fontface = "Arial";
    let fontsize = 40;

    let canvas = document.createElement('canvas');
    let context = canvas.getContext('2d');
    context.font = "Bold " + fontsize + "px " + fontface;

    let metrics = context.measureText( text );

    context.fillStyle = "white";
    context.strokeStyle = "black";
    context.lineWidth = 10;
    context.strokeText( text, 0, fontsize );
    context.fillText( text, 0, fontsize );

    let texture = new THREE.Texture(canvas) 
    texture.needsUpdate = true;

    let spriteMaterial = new THREE.SpriteMaterial( { map: texture } );
    this.sprite = new THREE.Sprite( spriteMaterial );

    this.sprite.scale.set(128,72,1.0);
  }
}

class Cursor {

  constructor(name, x, y, color){
    this.name = name;
    this.color = color;

    this.blockers = [];

    let map = new THREE.TextureLoader().load( "static/cursor.png" );
    let material = new THREE.SpriteMaterial( { map: map, color: color } );
    this.sprite = new THREE.Sprite( material );
    this.sprite.position.set(-40, 0, 800);
    this.sprite.scale.set(40, 40, 1);

    this.sprite.thisis = "cursor";

    this.text = new Text(name);
    this.text.sprite.position.set(0,0, 800);
  }

  setPos(newPos){
    this.pos = {x: newPos.x, y: newPos.y };
    this.sprite.position.set(newPos.x, newPos.y, 800);
    this.text.sprite.position.set(newPos.x+80,newPos.y-40, 800);
  }
}
