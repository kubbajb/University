#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <math.h>

const char *filename = "random.txt";
int count = 0;

void generate_and_save_random(void) {

  int i = 0;
  int j = 0;
  int n;
  int random;
  bool found;

  time_t t;                  // zarodek
  srand((unsigned)time(&t)); // zarodek

  printf("> Podaj n: ");
  scanf("%i", &n);

  while (n < 0 || n > 100000) {
    printf("> Podana liczba nie mieści się w zakresie 0 - 100000.‼️ \n>"
           " Podaj liczbę w podanym zakresie: ");
    scanf("%i", &n);
  }

  int generated_random_array[n];

  while (i < n) { // Dla każdego i, o ile i jest mniejsze od n
    random = rand() % (1000000 + 1 - 0) +
             0;    // Wygeneruj losową i przypisz ją zmiennej random
    found = false; // Reset found to false

    for (int j = 0; j < i; j++) { // Następnie dla każdego j mniejszego od i
      if (generated_random_array[j] ==
          random) {   // Sprawdź czy istnieje liczba identyczna do random
        found = true; // Jeśli tak, zmien wartość found na true
      }
    }

    if (found == false) {                 // Jeśli found ma wartość fałsz
      generated_random_array[i] = random; // zapisz random do szeregu
      i++; // Zwiększ i aby poszukać następnej liczby
    }

    else if (found == true) { // Jeśli found ma wartość prawda
      i--;                    // Zmniejsz aby poszukać jeszcze raz
    }
  }

  for (int j = 0; j < n; j++) {
    for (int i = 0; i < n; i++) {
      if (generated_random_array[i] > generated_random_array[i + 1]) {
        int temp = generated_random_array[i];
        generated_random_array[i] = generated_random_array[i + 1];
        generated_random_array[i + 1] = temp;
      }
    }
  }

  FILE *file;
  file = fopen("random.txt", "w+"); // Otwórz plik .txt

  for (int i = 0; i < n; i++) {
    fprintf(file, "%d\n",
            generated_random_array[i]); // Zapisz każdą liczbę z szeregu w pliku
  }

  fclose(file); // Zamknij plik .txt

  printf("> Wygenerowano %i przypadkowych, niepowtarzających się liczb."
         "\n> Wygenerowane "
         "liczby zapisano w pliku 'random.txt'.\n",
         n);
}
// generuje losowe n liczb i zapisuje w pliku .txt

void count_lines_in_file(void) {

  FILE *file;
  file = fopen(filename, "r");

  int c;

  for (c = getc(file); c != EOF; c = getc(file)) {
    if (c == '\n') // Licz ilość wystąpień znaku nowej linijki aż do końca pliku
                   // (EOF)
      count = count +
              1; // Jeśli znaleziono znak nowej linijki, zwiększ count o jeden
  }

  printf("> W pliku zapisanych jest %d liczb.\n", count);
  fclose(file);
}
// liczy linijki tekstu w pliku i zwraca wartość do zmiennej count
// na potrzeby kolejnej funkcji load_numbers_from_file()

void load_numbers_from_file(void) {

  FILE *file; // nadaje plikowi nazwę na potrzebę algorytmu ("file")
  file = fopen(filename, "r");

  struct stat sb;      // coś tam nie wiem
  stat(filename, &sb); // też nie wiem

  int loaded_random_numbers[count];
  int searched;
  int searched_index = 0;
  bool found_searched = false; // zmienne na potrzeby algorytmu

  char *new_number =
      malloc(sb.st_size); // rezerwuje miejsce w pamięci dla czytanej liczby
  int *array = malloc(
      (count) *
      sizeof(*array)); // a ilość liczb równa jest count z powyższego algorytmu

  for (int i = 0; i < count;
       i++) { // dla każdej iteracji czyta kolejną liczbę i zapisujemy w szeregu
    fscanf(file, "%d", &loaded_random_numbers[i]);
  }

  fclose(file); // zamyka plik

  printf("> Podaj liczbę do znalezienia: "); // pyta o liczbę do znalezienia
  scanf("%d", &searched);

  clock_t start1, end1;
  double total_time1;
  start1 = clock();

  int i = 1;
  while (i < count && loaded_random_numbers[i] <= searched) {
    i = i * 2;
  }

  int low = i / 2;
  int high = i;
  int mid = (high + low) / 2;

  while (low <= high && mid <= count) {
    mid = (low + high) / 2;

    if (loaded_random_numbers[mid] == searched) {
      searched_index = mid;
      found_searched = true;
      break;
    } else if (loaded_random_numbers[mid] < searched && mid <= count) {
      low = mid + 1;
    } else {
      high = mid - 1;
    }
  }

  if (found_searched == false) {
    for (int i = low; i <= count; i++) {
      if (loaded_random_numbers[i] == searched) {
        searched_index = i;
        found_searched = true;
        break;
      } else if (low == high) {
        found_searched = false;
      }
    }
  }

  total_time1 = ((double)(start1 - end1)) / 60;
  printf("> Zakończono wyszukiwanie, czas: %.2f.\n",
         fabs(total_time1));

  if (found_searched == true) { // jeśli znaleziono - komunikat z numerem
                                // indeksu
    printf("> Pozycja szukanej liczby w szeregu to %d.\n> Linijka w"
           " pliku to %d.\n"
           , searched_index, searched_index+1);
  } else if (found_searched == false) {
    printf("> Szukanej liczby nie ma w pliku!\n");   //jeśli nie znalezion
                                                         //  - smutny komunikat
  }
}
//ładuje przypadkowe liczby z pliku .txt
// i przeszukuje pod kątem liczby wprowadzonej przez użytkownika

int main(int argc, char *argv[]) {
  if (argc == 2) {
    if (strcmp(argv[1], "-random") == 0) {
      generate_and_save_random();
      return 1;
    }
    if (strcmp(argv[1], "-read") == 0) {
      count_lines_in_file();
      load_numbers_from_file();
      return 1;
    } else {
      printf("> Coś się popsuło... :(\n");
    }
  }
}