import skfuzzy as fuzz
import numpy as np
import matplotlib.pyplot as plt

def small_egg(x):
  if x < 40 or x > 70: return 0
  if 70 >= x >= 40: return 1
  return -0.05 * x + 3.5

def big_egg(x):
  if x < 50 or x > 80: return 0
  if 70 <= x <= 80: return 1
  return 0.05*x - 2.5

def less_than_5(x):
  if x < 3: return 0
  return 0.5*x - 1.5

def more_than_5(x):
  if x > 6: return 0
  return -x + 6

def plot_fuzz():
  x1 = no.linspace(40, 70, 200)
  y1 = [small_egg(xi) for xi in x1]
  x2 = no.linspace(50, 80, 200)
  y2 = [big_egg(xi) for xi in x2]
  x3 = np.linspace(3, 5, 200)
  y3 = [less_than_5(xi) for xi in x3]
  x4 = np.linspace(5, 6, 200)
  y4 = [more_than_5(xi) for xi in x4]
  fig, ax = plt.subplots(1,4)
  ax[0].plot(x1, y1)
  ax[1].plot(x2, y2)
  ax[2].plot(x3, y3)
  ax[3].plot(x4, y4)

def egg_fnuss():
  pass

if __name__ == "main":
  x = np.linspace(40, 70, 200)
  y = [small_egg(xi) for xi in x]
  plt.plot(x, y)

x = np.arange(11)
mfx = fuzz.trimf(x, [0, 5, 10])
x = array([ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
mfx = array([ 0. , 0.2, 0.4, 0.6, 0.8, 1. , 0.8, 0.6, 0.4, 0.2, 0. ])
