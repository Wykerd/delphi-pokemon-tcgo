function Updater() {
  this.items = [];

  this.link = function (id, store, callback) {
    var item = this.items.find(function (x) {
      return x.id === id;
    });

    if (item) {
      item = {
        id: id,
        update: callback,
        store: store
      };
      return;
    }

    this.items.push({
      id: id,
      update: callback,
      store: store
    });
  };

  this.store = function (id, name, data) {
    var item = this.items.find(function (x) {
      return x.id === id;
    });
    item ? Object.assign(item.store, {
      name: name,
      data: data
    }) : console.warn('Could not find item to store');
  };

  this.update = function (id, data) {
    var item = this.items.find(function (x) {
      return x.id === id;
    });
    if (!item) return console.warn('Could not find item to update');

    for (var prop in data) {
      item.store[prop] = data[prop];
    }

    console.log(data);
    item.update(item.store);
  };
}

var updater = new Updater();