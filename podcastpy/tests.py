import unittest

from pyramid import testing


class ViewTests(unittest.TestCase):
    def setUp(self):
        self.config = testing.setUp()

    def tearDown(self):
        testing.tearDown()

    def test_my_view(self):
        request = testing.DummyRequest()
        from podcastpy.views.default import hello_world
        info = hello_world(request)
        self.assertEqual(b'Hello World!', info.body)


class FunctionalTests(unittest.TestCase):
    def setUp(self):
        from podcastpy import main
        app = main({})
        from webtest import TestApp
        self.testapp = TestApp(app)

    def test_root(self):
        res = self.testapp.get('/test', status=200)
        self.assertTrue(b'Hello World' in res.body)
