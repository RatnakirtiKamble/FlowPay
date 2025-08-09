import { Routes, Route } from 'react-router-dom';
import Home from './pages/Home';
import Header from './Components/Header';
import Footer from './Components/Footer';
import Layout from './Layout/Layout';

function App() {
  return (
      <Layout>
        <Routes>
          <Route path="/" element={<Home />} />
        </Routes>
      </Layout>

  );
}

export default App;
