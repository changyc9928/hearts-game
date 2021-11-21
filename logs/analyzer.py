import csv
import glob, os

def get_data(filename):
	user_points = []
	enemy_points = []

	user_route = "monteCarlo/"
	enemy_route = "probability/"

	with open(user_route + filename, "rt") as file:
		data = csv.reader(file)
		for row in data:
			user_points.append(int(row[2]))

	with open(enemy_route + filename, "rt") as file:
		data = csv.reader(file)
		for row in data:
			enemy_points.append(int(row[2]))

	return sum(user_points), sum(enemy_points)

if __name__ == '__main__':
	current_dir = os.getcwd()
	dir_files = current_dir + "/probability/"
	
	os.chdir(dir_files)
	files = glob.glob('*.csv')
	os.chdir(current_dir)
	
	user_win = 0
	enemy_win = 0
	
	for file in files:
		user, enemy = get_data(file)

		if user >= 100 or enemy >= 100:
			print("user: {user}, enemy: {enemy}".format(user=user, enemy=enemy))
		
			if user < enemy:
			        user_win += 1
			else:
				enemy_win += 1

	print("User (Monte Carlo) won: {} times | Enemy (probability) won: {} times | Total rounds: {} rounds".format(user_win, enemy_win, user_win+enemy_win))

