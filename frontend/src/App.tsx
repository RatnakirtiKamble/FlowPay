import { Routes, Route } from 'react-router-dom';
import Home from './pages/Home';
import Layout from './Layout/Layout';
import Dashboard from './pages/Dashboard';
import ProtectedRoute from './Components/ProtectedRoute';
import { AuthProvider } from './Context/AuthContext'; // Import the provider
import DocsPage from './pages/DocsPage';
import DemoMerchantSite from './pages/Demo';

function App() {
  return (
    <AuthProvider> 
      <Layout>
        <Routes>
          <Route path="/" element={<Home />} />
          <Route path="docs" element={<DocsPage />}/>
          <Route path="/dashboard" element={
            <ProtectedRoute>
              <Dashboard />
            </ProtectedRoute>
          } />
          <Route path="/demo" element={<DemoMerchantSite/>} />
        </Routes>
      </Layout>
    </AuthProvider>
  );
}

export default App;