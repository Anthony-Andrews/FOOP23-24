/*
 * File: Wordle.java
 * -----------------
 * This module is the starter file for the Wordle assignment.
 * BE SURE TO UPDATE THIS COMMENT WHEN YOU COMPLETE THE CODE.
 */

// Written by Anthony Andrews 5/20/2024, with help from Github Copilot.

import edu.willamette.cs1.wordle.WordleDictionary;
import edu.willamette.cs1.wordle.WordleGWindow;

public class Wordle {

    public void run() {
        gw = new WordleGWindow();
        gw.addEnterListener((s) -> enterAction(s));
    }

/*
 * Called when the user hits the RETURN key or clicks the ENTER button,
 * passing in the string of characters on the current row.
 */

    public void enterAction(String s) {
        if (s.toLowerCase().equals(SecretWord)){
            colorBoxes(i, s);
            colorKeys(s);
            gw.showMessage("Won in "+(i+1));
        }
        else if(validWord(s)){
            colorBoxes(i, s);
            colorKeys(s);
            gw.setCurrentRow(i+1);
            i++;
            System.out.println(i);
        }
        else{
            gw.showMessage("Invalid Word");
        }
    }

    public void displayWord(int row, String word) {
        for (int i = 0; i < word.length(); i++) {
            gw.setSquareLetter(row, i, word.substring(i, i + 1));
        }
    }

    public boolean validWord(String word){
        for (int i = 0; i < WordleDictionary.FIVE_LETTER_WORDS.length; i++){
            if (word.toLowerCase().equals(WordleDictionary.FIVE_LETTER_WORDS[i])){
                return true;
            }
        }
        return false;
    
    }

    public void colorBoxes(int row, String word){
        String lowerCase = word.toLowerCase();
        for (int i = 0; i < lowerCase.length(); i++){
            if (lowerCase.substring(i, i + 1).equals(SecretWord.substring(i, i + 1))){
                gw.setSquareColor(row, i, WordleGWindow.CORRECT_COLOR);
            } else if (SecretWord.contains(lowerCase.substring(i, i + 1))){
                gw.setSquareColor(row, i, WordleGWindow.PRESENT_COLOR);
            } else {
                gw.setSquareColor(row, i, WordleGWindow.MISSING_COLOR);
            }
        }
    
    }

    public void colorKeys(String word){
        String lowerCase = word.toLowerCase();
        for (int i = 0; i < word.length(); i++){
            if  (lowerCase.substring(i, i + 1).equals(SecretWord.substring(i, i + 1))){
                gw.setKeyColor(word.substring(i, i + 1), WordleGWindow.CORRECT_COLOR);
            }
            else if (SecretWord.contains(lowerCase.substring(i, i + 1))){
                gw.setKeyColor(word.substring(i, i + 1), WordleGWindow.PRESENT_COLOR);
            } else {
                gw.setKeyColor(word.substring(i, i + 1), WordleGWindow.MISSING_COLOR);
            }
        }
    
    }

/* Startup code */

    public static void main(String[] args) {

        new Wordle().run();

        SecretWord =
            WordleDictionary.FIVE_LETTER_WORDS[
                (int) (WordleDictionary.FIVE_LETTER_WORDS.length * Math.random())
            ];
        System.out.println(SecretWord);
    }

/* Private instance variables */

    private WordleGWindow gw;
    static String SecretWord;
    private int i = 0;

}
