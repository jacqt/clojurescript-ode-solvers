from flask.ext.cors import CORS
import flask
import os
import csv

current_dir = os.path.dirname(os.path.realpath(__file__))
parent_dir = os.path.dirname(current_dir)
data_dir = os.path.join(parent_dir, "static-site", "data")

app = flask.Flask(__name__)
CORS(app)


def reduce_to_json(json_data, next_data):
  labels = ["t", "y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8"]
  next_field = {}
  for i in range(len(next_data)):
    next_field[labels[i]] = next_data[i]
  json_data["data"].append(next_field)
  return json_data

@app.route("/data_files")
def data_files():
  body = {"data_files": os.listdir(data_dir)}
  return flask.jsonify(**body)


@app.route("/data_files/<filename>")
def get_data(filename):
  with open(os.path.join(data_dir, filename)) as fopen:
    reader = csv.reader(fopen)
    body = reduce(reduce_to_json, reader, {"data": []})
    print body
    return flask.jsonify(body)

if __name__ == "__main__":
  app.run(host='0.0.0.0', port='2589')
