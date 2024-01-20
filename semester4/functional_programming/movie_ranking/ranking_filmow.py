# Import niezbędnych bibliotek

import requests
from bs4 import BeautifulSoup
import matplotlib.pyplot as plt
import pandas as pd
import plotly.express as px

# Zdefiniowanie źródła danych

url = 'https://www.boxofficemojo.com/chart/top_lifetime_gross/?ref_=bo_cso_ac_tab_toplifetimegross'

# Zebranie źródła strony w formacie .html

def parse(url):
  response = requests.get(url)
  soup = BeautifulSoup(response.content, 'html.parser')
  return soup

# Zebranie danych z html i utworzenie pliku dict zawierającej filmy i zysk dla wytworni

def pull_table(soup):
  rows = soup.find_all('tr')[1:] 
  movies = []
  grosses = []
  
  for row in rows:
      cells = row.find_all('td')
      title = cells[1].find('a').text
      gross = int(cells[2].text.replace('$', '').replace(',', ''))
      movies.append(title)
      grosses.append(gross)
      
  movies = [movie.replace('\xa0', '') for movie in movies]
  grosses = [gross/1000000000 for gross in grosses]
  data = {'movies': movies, 'grosses': grosses}
  
  return data

# Transformacja pliku dict w tabelę

def dict_to_df(data):
  data = pd.DataFrame(data)
  return data

# Utworzenie wykresu dla 10 filmów o najmniejszych dochodach

def plot_lowest(data):
  data_low = data.sort_values('grosses', ascending=False).nsmallest(10, 'grosses')
  plot_l = px.bar(data_low, x='grosses', y='movies')
  data_low = pd.DataFrame(data=data_low)
  return plot_l.show()

# Utworzenie wykresu dla 10 filmów o największych dochodach

def plot_highest(data):
  data_high = data.sort_values('grosses', ascending=True).nlargest(10, 'grosses')
  plot_h = px.bar(data_high, x='grosses', y='movies')
  data_high = pd.DataFrame(data=data_high)
  return plot_h.show()

# Utworzenie histogramu dla wszystkich filmów w zestawie danych

def plot_hist(data):
  plot_his = px.histogram(data, x="grosses")
  return plot_his.show()

# Utworzenie wykresu dla wszystkich filmów w zestawie danych

def plot_tot(data):
  plot_tot = px.bar(data, x='grosses', y='movies')
  return plot_tot.show()

# Wywoływanie pojedynczych funkcji przygotowujących dane

soup = parse(url)

data = pull_table(soup)

data = dict_to_df(data)

# Wywoływanie pojedynczych funkcji przygotowujących wykresy

plot_lowest(data)

plot_highest(data)

plot_hist(data)

plot_tot(data)
