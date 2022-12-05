from app import app


@app.route('/')
@app.route('/index')
def index():
    user = {'username': "Jakub"}
    return '''
<html>
    <head>
        <title>Home Page - Kalaha</title>
    </head>
    <body>
        <h1>Hello, ''' + user['username'] + '''!</h1>
    </body>
</html>'''
