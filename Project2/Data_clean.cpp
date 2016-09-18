#include<iostream>
#include<fstream>
#include<math.h>
#include<string>
#include<vector>
#include<cstdlib>

using namespace std;

// This function sparse a string by a delimiter. It has been downloaded from:
// http://www.sbin.org/doc/HOWTO/C++Programming-HOWTO-7.html
void Tokenize(const string& str, vector<string>& tokens, const string& delimiters = " ")
{
    // Skip delimiters at beginning.
    string::size_type lastPos = str.find_first_not_of(delimiters, 0);
    // Find first "non-delimiter".
    string::size_type pos = str.find_first_of(delimiters, lastPos);

    while (string::npos != pos || string::npos != lastPos)
    {
        // Found a token, add it to the vector.
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        // Skip delimiters.  Note the "not_of"
        lastPos = str.find_first_not_of(delimiters, pos);
        // Find next "non-delimiter"
        pos = str.find_first_of(delimiters, lastPos);
    }
}

// This function trims extra spaces from the begining and end of a string
std::string trim(string& str)
{
    size_t strBegin = str.find_first_not_of(" ");
    size_t strEnd = str.find_last_not_of(" ");
    size_t strRange = strEnd - strBegin + 1;

    return str.substr(strBegin, strRange);
}

int main ()
{
    bool first_time = false;
    //int number_of_stars_to_be_analyzied = 7000;
    
    if ( first_time )
    {
	// Concatenation of two files
	std::ifstream file1("actor_movies.txt", std::ios_base::binary);
	std::ifstream file2("actress_movies.txt", std::ios_base::binary);
	std::ofstream file3("combined_list.txt", std::ios_base::binary);
	file3 << file1.rdbuf() << file2.rdbuf();
    }

    std::ifstream file("combined_list.txt");
    std::string str;
    
    vector<string> movies;
    vector<string> stars;
    vector< vector <int> > stars_contribution;
    vector<int> num_stars;

    vector< vector<string> > edited_list;
    
    while (std::getline(file, str))
    {
	str = trim ( str );
	vector<string> tokens;
	Tokenize ( str , tokens , "\t\t" );
	if ( tokens.size() > 20 )
	{
	    vector<string> entry;
	    for ( int i = 0 ; i < tokens.size() ; i++ )
	    {
		entry.push_back ( trim ( tokens[i] ) );
	    }
	    edited_list.push_back ( entry );
	}
    }
/*
    std::ofstream out("cleaned_data.txt");
    for ( int i = 0 ; i < edited_list.size() ; i++ )
    {
	for ( int j = 0 ; j < (edited_list[i].size()-1) ; j++ )
	{
	    out << edited_list[i][j] << '\t';
	}
	out << edited_list[i][(edited_list[i].size()-1)] << endl;
    }
    out.close();
*/

    int i = 0;
    while ( i < edited_list.size() )
    {
	stars.push_back ( edited_list[i][0] );
	vector<int> row;
	for ( int j = 1 ; j < edited_list[i].size() ; j++ )
	{
	    string movie = edited_list[i][j];
	    std::vector<string>::iterator it = find(movies.begin(), movies.end(), movie);
	    if ( it == movies.end())
	    {
		// not found
		movies.push_back ( movie );
		row.push_back ( movies.size()-1 );
		num_stars.push_back ( 1 );
	    }
	    else
	    {
		// found
		int pos = it - movies.begin();
		row.push_back ( pos );
		num_stars[pos] = num_stars[pos] + 1;
	    }
	}
	stars_contribution.push_back ( row );
	cout << i << " " << edited_list[i][0] <<  " " << edited_list[i].size()-1 << endl;
	i++;
    }
    

    for ( int i = 0 ; i < edited_list.size() ; i++ ) edited_list[i].clear();
    edited_list.clear();
    
    std::ofstream out_stars("stars_id.txt");
    for ( int i = 0 ; i < stars.size() ; i++ )
    {
	out_stars << i << '\t' << stars[i] << endl;
    }
    out_stars.close();
    
    std::ofstream out_movies("movies_id.txt");
    for ( int i = 0 ; i < movies.size() ; i++ )
    {
	out_movies << i << '\t' << movies[i] << endl;
    }
    out_movies.close();
    
    std::ofstream out_data("data_list.txt");
    for ( int i = 0 ; i < stars.size() ; i++ )
    {
	for ( int j = 0 ; j < (stars_contribution[i].size()-1) ; j++ )
	{
	    out_data << stars_contribution[i][j] << '\t';
	}
	out_data << stars_contribution[i][(stars_contribution[i].size()-1)] << endl;
    }
    out_data.close();

    return 0;
}
