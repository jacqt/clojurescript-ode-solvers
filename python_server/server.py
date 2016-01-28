import flask
import os

current_dir = os.path.dirname(os.path.realpath(__file__))
parent_dir = os.path.dirname(current_dir)
data_dir = os.path.join(parent_dir, "static-site", "data")

app = flask.Flask(__name__)

def add_data(file_name):
  return "data/" + file_name

@app.route("/data_files")
def data_files():
  body = {"data_files": map(add_data, os.listdir(data_dir))}
  return flask.jsonify(**body)

if __name__ == "__main__":
  app.run()
