require( './styles/main.scss' );
var firebase = require('firebase');

var config = {
    apiKey: "AIzaSyAylmlOCFnepoL8WVaNa9NDI50k29TFaJg",
    authDomain: "elmmeetup.firebaseapp.com",
    databaseURL: "https://elmmeetup.firebaseio.com",
    storageBucket: "elmmeetup.appspot.com",
    messagingSenderId: "721791476906"
};

var DEVICESPATH = "devices"

var fb = firebase.initializeApp(config);

var Elm = require( '../elm/Main' );
var app = Elm.Main.embed( document.getElementById( 'main' ) );

var database = fb.database();

function cleanDevice (device) {
  device.type = device.type_;
  delete device.type_;
  delete device.id;
  return device;
}

function addDevice(device){
  device = cleanDevice(device);
  var promise = database
    .ref(DEVICESPATH)
    .push(device);
  return promise;
}

function updateDevice(device){
  var id = device.id;
  device = cleanDevice(device);
  var promise = database
    .ref(DEVICESPATH + "/" + id)
    .set(device);
  return promise;
}

function deviceListener(){
  return database.ref(DEVICESPATH);
}


app.ports.addNewDevice.subscribe(function(device){
  addDevice(device)
    .then(function(response){
      app.ports.deviceAdded.send(null);
    }, function(err){
      console.log("error:", err);
    });
});

app.ports.updateDevice.subscribe(function(device){
  updateDevice(device)
    .then(function(response){
      app.ports.deviceUpdated.send(null);
    }, function(err){
      console.log("error:", err);
    });
});

var listener = deviceListener();
listener.on("child_added", function(data){
  var device = data.val();
  var id = data.key;
  device.type_ = device.type;
  device.id = id;
  delete device.type;
  app.ports.newDeviceAdded.send(device);
});

listener.on("child_changed", function(data){
  var device = data.val();
  var id = data.key;
  device.type_ = device.type;
  device.id = id;
  delete device.type;
  app.ports.deviceChanged.send(device);
});
