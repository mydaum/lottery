Feature:
    Lotteries can be created

    Background:
        Given that I have started lottery

    Scenario: When you successfully create a lottery then you get a url and a
        deletion token back.
        When I create a lottery
        Then I get a 201 response code
        And I get a URL
        And I get a deletion token
